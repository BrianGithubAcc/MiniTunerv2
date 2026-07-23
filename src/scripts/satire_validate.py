#!/usr/bin/env python3
"""Rigorous batch validation for MiniTuner's SATIRE-assisted path.

SATIRE v1.1 supplies the shared expression-DAG representation used by the
project, but its public first-order result is not by itself a complete error
bound.  This module evaluates the same DAG with directed MPFR intervals.  The
computed interval contains arbitrary implementation epsilon/delta noise and
all nonlinear, second-order, and cross-site effects. A query that cannot be
enclosed is returned as ambiguous; SATIRE modes never silently switch to
FPTaylor.
"""

from __future__ import annotations

import argparse
import ast
import concurrent.futures
import copy
import functools
import io
import json
import math
import os
from pathlib import Path
import re
import socket
import sys
import time
import tokenize
from typing import Any, Callable

try:
    import gmpy2
except ImportError as error:  # pragma: no cover - exercised by fallback path
    raise SystemExit(f"gmpy2 is required for SATIRE hybrid validation: {error}")


PRECISION = 256
U = gmpy2.mpfr(2) ** -53
ETA = gmpy2.mpfr(2) ** -1075
MAX_FINITE = gmpy2.mpfr("1.797693134862315708145274237317043567981e308")


def verify_satire_extension() -> str:
    """Verify that the pinned SATIRE custom-noise extension is available."""
    satire_root = os.environ.get("SATIRE_PATH")
    if not satire_root:
        return "SATIRE_PATH is not configured"
    source = str(Path(satire_root) / "src")
    if source not in sys.path:
        sys.path.insert(0, source)
    try:
        import ASTtypes  # type: ignore
    except Exception as error:  # pragma: no cover - environment fallback
        return f"SATIRE import failed: {error}"
    if not hasattr(ASTtypes.AST, "set_custom_noise"):
        return "SATIRE arbitrary epsilon/delta extension is unavailable"
    return ""


def directed(rounding: int, function: Callable[..., Any], *args: Any) -> Any:
    context = gmpy2.get_context().copy()
    context.precision = PRECISION
    context.round = rounding
    with gmpy2.context(context):
        return function(*args)


def down(function: Callable[..., Any], *args: Any) -> Any:
    return directed(gmpy2.RoundDown, function, *args)


def up(function: Callable[..., Any], *args: Any) -> Any:
    return directed(gmpy2.RoundUp, function, *args)


class Unresolved(Exception):
    pass


class InvalidImplementationDomain(Exception):
    pass


class InvalidConfiguration(Exception):
    """The selected error specification cannot guarantee finite execution."""


class Interval:
    __slots__ = ("lo", "hi")

    def __init__(self, lo: Any, hi: Any):
        self.lo = gmpy2.mpfr(lo)
        self.hi = gmpy2.mpfr(hi)
        if self.lo > self.hi or not gmpy2.is_finite(self.lo) or not gmpy2.is_finite(self.hi):
            raise Unresolved("non-finite or reversed interval")

    @classmethod
    def point(cls, value: Any) -> "Interval":
        return cls(value, value)

    def magnitude(self) -> Any:
        return max(abs(self.lo), abs(self.hi))

    def minimum_magnitude(self) -> Any:
        if self.lo <= 0 <= self.hi:
            return gmpy2.mpfr(0)
        return min(abs(self.lo), abs(self.hi))

    def expand(self, radius: Any) -> "Interval":
        if radius < 0 or not gmpy2.is_finite(radius):
            raise Unresolved("invalid error radius")
        return Interval(
            down(lambda a, b: a - b, self.lo, radius),
            up(lambda a, b: a + b, self.hi, radius),
        )


def iadd(a: Interval, b: Interval) -> Interval:
    return Interval(down(lambda x, y: x + y, a.lo, b.lo), up(lambda x, y: x + y, a.hi, b.hi))


def isub(a: Interval, b: Interval) -> Interval:
    return Interval(down(lambda x, y: x - y, a.lo, b.hi), up(lambda x, y: x - y, a.hi, b.lo))


def imul(a: Interval, b: Interval) -> Interval:
    lower = [
        down(lambda x, y: x * y, x, y)
        for x in (a.lo, a.hi) for y in (b.lo, b.hi)
    ]
    upper = [
        up(lambda x, y: x * y, x, y)
        for x in (a.lo, a.hi) for y in (b.lo, b.hi)
    ]
    return Interval(min(lower), max(upper))


def idiv(a: Interval, b: Interval) -> Interval:
    if b.lo <= 0 <= b.hi:
        raise Unresolved("denominator interval contains zero")
    reciprocal = Interval(
        down(lambda x: 1 / x, b.hi),
        up(lambda x: 1 / x, b.lo),
    )
    return imul(a, reciprocal)


def ipow(base: Interval, exponent: Interval) -> Interval:
    if exponent.lo != exponent.hi or not gmpy2.is_integer(exponent.lo):
        raise Unresolved("only constant integer powers are certified")
    power = int(exponent.lo)
    if power < 0:
        return idiv(Interval.point(1), ipow(base, Interval.point(-power)))
    if power == 0:
        return Interval.point(1)
    if power % 2 == 0:
        values = [
            down(lambda x: x ** power, base.lo),
            down(lambda x: x ** power, base.hi),
        ]
        lo = gmpy2.mpfr(0) if base.lo <= 0 <= base.hi else min(values)
        hi = max(
            up(lambda x: x ** power, base.lo),
            up(lambda x: x ** power, base.hi),
        )
        return Interval(lo, hi)
    return Interval(
        down(lambda x: x ** power, base.lo),
        up(lambda x: x ** power, base.hi),
    )


def unary(name: str, value: Interval) -> Interval:
    if name == "inv":
        return idiv(Interval.point(1), value)
    if name in {"abs", "fabs"}:
        if value.lo >= 0:
            return value
        if value.hi <= 0:
            return Interval(-value.hi, -value.lo)
        return Interval(0, max(-value.lo, value.hi))
    if name == "exp":
        result = Interval(down(gmpy2.exp, value.lo), up(gmpy2.exp, value.hi))
    elif name == "log":
        if value.lo <= 0:
            raise Unresolved("logarithm interval is not strictly positive")
        result = Interval(down(gmpy2.log, value.lo), up(gmpy2.log, value.hi))
    elif name == "sqrt":
        if value.lo < 0:
            raise Unresolved("square-root interval is negative")
        result = Interval(down(gmpy2.sqrt, value.lo), up(gmpy2.sqrt, value.hi))
    elif name == "atan":
        result = Interval(down(gmpy2.atan, value.lo), up(gmpy2.atan, value.hi))
    elif name in {"sin", "cos"}:
        # A full-period enclosure is deliberately conservative and rigorous.
        result = Interval(-1, 1)
    elif name == "tan":
        pi = up(gmpy2.const_pi)
        left = down(lambda x: (x - pi / 2) / pi, value.lo)
        right = up(lambda x: (x - pi / 2) / pi, value.hi)
        if gmpy2.ceil(left) <= gmpy2.floor(right):
            raise Unresolved("tangent interval crosses a pole")
        result = Interval(down(gmpy2.tan, value.lo), up(gmpy2.tan, value.hi))
    else:
        raise Unresolved(f"unsupported operation {name}")
    if result.magnitude() > MAX_FINITE:
        raise Unresolved("binary64 overflow")
    return result


def evaluate(node: ast.AST, environment: dict[str, Interval]) -> Interval:
    if isinstance(node, ast.Expression):
        return evaluate(node.body, environment)
    if isinstance(node, ast.Name):
        try:
            return environment[node.id]
        except KeyError:
            raise Unresolved(f"unknown value {node.id}") from None
    if isinstance(node, ast.Constant) and isinstance(node.value, (int, float)):
        return Interval.point(str(node.value))
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
        value = evaluate(node.operand, environment)
        return Interval(-value.hi, -value.lo)
    if isinstance(node, ast.BinOp):
        left = evaluate(node.left, environment)
        right = evaluate(node.right, environment)
        if isinstance(node.op, ast.Add):
            return iadd(left, right)
        if isinstance(node.op, ast.Sub):
            return isub(left, right)
        if isinstance(node.op, ast.Mult):
            return imul(left, right)
        if isinstance(node.op, ast.Div):
            return idiv(left, right)
        if isinstance(node.op, ast.Pow):
            return ipow(left, right)
    if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
        if (
            node.func.id == "mp"
            and len(node.args) == 1
            and isinstance(node.args[0], ast.Constant)
            and isinstance(node.args[0].value, str)
        ):
            return Interval.point(node.args[0].value)
        if len(node.args) == 1:
            return unary(node.func.id, evaluate(node.args[0], environment))
        if node.func.id == "hypot" and len(node.args) == 2:
            left = evaluate(node.args[0], environment)
            right = evaluate(node.args[1], environment)
            return unary("sqrt", iadd(imul(left, left), imul(right, right)))
    raise Unresolved(f"unsupported expression node {ast.dump(node)}")


def expression_key(node: ast.AST) -> str:
    return ast.dump(node, annotate_fields=True, include_attributes=False)


def expand_expression(node: ast.AST, environment: dict[str, ast.AST]) -> ast.AST:
    """Substitute SSA names with their real symbolic expressions."""
    if isinstance(node, ast.Name) and node.id in environment:
        replacement = environment[node.id]
        if isinstance(replacement, ast.Expression):
            replacement = replacement.body
        return copy.deepcopy(replacement)
    replacement = copy.deepcopy(node)
    for field, value in ast.iter_fields(replacement):
        if isinstance(value, ast.AST):
            setattr(replacement, field, expand_expression(value, environment))
        elif isinstance(value, list):
            setattr(
                replacement,
                field,
                [
                    expand_expression(item, environment)
                    if isinstance(item, ast.AST)
                    else item
                    for item in value
                ],
            )
    return replacement


def multiplication_factors(node: ast.AST) -> list[ast.AST]:
    if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Mult):
        return multiplication_factors(node.left) + multiplication_factors(node.right)
    return [node]


def correlated_evaluate(node: ast.AST, environment: dict[str, Interval]) -> Interval:
    """Rigorous interval evaluation with safe dependency-preserving rewrites."""
    if isinstance(node, ast.Expression):
        return correlated_evaluate(node.body, environment)
    if isinstance(node, ast.Name):
        return environment[node.id]
    if isinstance(node, ast.Constant) and isinstance(node.value, (int, float)):
        return Interval.point(str(node.value))
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
        value = correlated_evaluate(node.operand, environment)
        return Interval(-value.hi, -value.lo)
    if isinstance(node, ast.BinOp):
        if isinstance(node.op, ast.Sub) and expression_key(node.left) == expression_key(
            node.right
        ):
            return Interval.point(0)
        if isinstance(node.op, ast.Div) and expression_key(node.left) == expression_key(
            node.right
        ):
            denominator = correlated_evaluate(node.right, environment)
            if denominator.minimum_magnitude() == 0:
                raise Unresolved("self-division may contain zero")
            return Interval.point(1)
        if isinstance(node.op, ast.Mult):
            # Multiplication is associative over the real expression. Group
            # repeated factors before interval evaluation so y*y is [0,m²],
            # not [-m²,m²], even when the source was parsed as (x*y)*y.
            grouped: dict[str, tuple[ast.AST, int]] = {}
            for factor in multiplication_factors(node):
                key = expression_key(factor)
                original, count = grouped.get(key, (factor, 0))
                grouped[key] = (original, count + 1)
            result = Interval.point(1)
            for factor, count in grouped.values():
                value = correlated_evaluate(factor, environment)
                result = imul(result, ipow(value, Interval.point(count)))
            return result
        left = correlated_evaluate(node.left, environment)
        right = correlated_evaluate(node.right, environment)
        if isinstance(node.op, ast.Add):
            return iadd(left, right)
        if isinstance(node.op, ast.Sub):
            return isub(left, right)
        if isinstance(node.op, ast.Div):
            return idiv(left, right)
        if isinstance(node.op, ast.Pow):
            return ipow(left, right)
    if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
        name = node.func.id
        if (
            name == "mp"
            and len(node.args) == 1
            and isinstance(node.args[0], ast.Constant)
        ):
            return Interval.point(node.args[0].value)
        if len(node.args) == 1:
            argument = node.args[0]
            # This identity is exact for the real DAG. Binary64 overflow is
            # still checked at the exp definition before log is evaluated.
            if (
                name == "log"
                and isinstance(argument, ast.Call)
                and isinstance(argument.func, ast.Name)
                and argument.func.id == "exp"
                and len(argument.args) == 1
            ):
                return correlated_evaluate(argument.args[0], environment)
            return unary(name, correlated_evaluate(argument, environment))
        if name == "hypot" and len(node.args) == 2:
            left = correlated_evaluate(node.args[0], environment)
            right = correlated_evaluate(node.args[1], environment)
            return unary(
                "sqrt",
                iadd(
                    ipow(left, Interval.point(2)),
                    ipow(right, Interval.point(2)),
                ),
            )
    raise Unresolved(f"unsupported correlated expression {ast.dump(node)}")


def ineg(value: Interval) -> Interval:
    return Interval(-value.hi, -value.lo)


def linear_add(
    left: dict[str, Interval],
    right: dict[str, Interval],
    *,
    subtract: bool = False,
) -> dict[str, Interval]:
    result = dict(left)
    for name, coefficient in right.items():
        coefficient = ineg(coefficient) if subtract else coefficient
        result[name] = (
            iadd(result[name], coefficient) if name in result else coefficient
        )
    return result


def linear_scale(
    coefficients: dict[str, Interval], factor: Interval
) -> dict[str, Interval]:
    return {
        name: imul(factor, coefficient)
        for name, coefficient in coefficients.items()
    }


def linear_radius(coefficients: dict[str, Interval]) -> Any:
    return add_up(*(coefficient.magnitude() for coefficient in coefficients.values()))


class Tracked:
    """A rigorous scalar enclosure plus a signed affine error form.

    ``error`` is the former emergency scalar radius. ``linear`` maps stable
    rounding-site IDs to signed coefficient intervals and ``remainder``
    encloses nonlinear products and propagated non-affine terms. Both bounds
    are rigorous, so callers may safely use their minimum.
    """

    __slots__ = ("real", "error", "linear", "remainder")

    def __init__(
        self,
        real: Interval,
        error: Any = 0,
        linear: dict[str, Interval] | None = None,
        remainder: Any = 0,
    ):
        self.real = real
        self.error = gmpy2.mpfr(error)
        self.linear = {} if linear is None else linear
        self.remainder = gmpy2.mpfr(remainder)
        if self.error < 0 or not gmpy2.is_finite(self.error):
            raise Unresolved("invalid propagated error")
        if self.remainder < 0 or not gmpy2.is_finite(self.remainder):
            raise Unresolved("invalid nonlinear remainder")

    def affine_radius(self) -> Any:
        return add_up(linear_radius(self.linear), self.remainder)

    def bound(self) -> Any:
        return min(self.error, self.affine_radius())

    def approximate_interval(self) -> Interval:
        return self.real.expand(self.bound())

    def add_noise(self, name: str, radius: Any) -> "Tracked":
        coefficients = dict(self.linear)
        coefficient = Interval(0, radius)
        coefficients[name] = (
            iadd(coefficients[name], coefficient)
            if name in coefficients
            else coefficient
        )
        return Tracked(
            self.real,
            add_up(self.error, radius),
            coefficients,
            self.remainder,
        )


def add_up(*values: Any) -> Any:
    result = gmpy2.mpfr(0)
    for value in values:
        result = up(lambda x, y: x + y, result, value)
    return result


def mul_up(*values: Any) -> Any:
    result = gmpy2.mpfr(1)
    for value in values:
        result = up(lambda x, y: x * y, result, value)
    return result


def unary_lipschitz(name: str, expanded: Interval) -> Any:
    if name in {"sin", "cos", "abs", "fabs", "atan"}:
        return gmpy2.mpfr(1)
    if name == "exp":
        return up(gmpy2.exp, expanded.hi)
    if name == "log":
        if expanded.lo <= 0:
            raise InvalidConfiguration(
                "perturbed logarithm input may be nonpositive"
            )
        return up(lambda x: 1 / x, expanded.lo)
    if name == "sqrt":
        if expanded.lo < 0:
            raise InvalidConfiguration(
                "perturbed square-root input may be negative"
            )
        if expanded.lo == 0:
            raise Unresolved("square-root derivative is unresolved at zero")
        return up(lambda x: 1 / (2 * gmpy2.sqrt(x)), expanded.lo)
    if name == "tan":
        tangent = unary("tan", expanded)
        return up(lambda x: 1 + x * x, tangent.magnitude())
    if name == "inv":
        minimum = expanded.minimum_magnitude()
        if minimum == 0:
            raise InvalidConfiguration(
                "perturbed reciprocal input may contain zero"
            )
        return up(lambda x: 1 / (x * x), minimum)
    raise Unresolved(f"unsupported operation {name}")


def unary_derivative(name: str, expanded: Interval) -> Interval:
    """Enclose f' over the complete perturbed input interval."""
    if name == "exp":
        return unary("exp", expanded)
    if name == "log":
        if expanded.lo <= 0:
            raise InvalidConfiguration(
                "perturbed logarithm input may be nonpositive"
            )
        return idiv(Interval.point(1), expanded)
    if name == "sqrt":
        if expanded.lo <= 0:
            raise Unresolved("square-root derivative is unresolved at zero")
        return idiv(
            Interval.point(1),
            imul(Interval.point(2), unary("sqrt", expanded)),
        )
    if name == "inv":
        if expanded.minimum_magnitude() == 0:
            raise InvalidConfiguration(
                "perturbed reciprocal input may contain zero"
            )
        return ineg(idiv(Interval.point(1), ipow(expanded, Interval.point(2))))
    if name == "sin":
        return Interval(-1, 1)
    if name == "cos":
        return Interval(-1, 1)
    if name == "tan":
        tangent = unary("tan", expanded)
        return iadd(Interval.point(1), ipow(tangent, Interval.point(2)))
    if name == "atan":
        denominator = iadd(
            Interval.point(1), ipow(expanded, Interval.point(2))
        )
        return idiv(Interval.point(1), denominator)
    if name in {"abs", "fabs"}:
        if expanded.lo >= 0:
            return Interval.point(1)
        if expanded.hi <= 0:
            return Interval.point(-1)
        return Interval(-1, 1)
    raise Unresolved(f"unsupported derivative for {name}")


def unary_tracked(name: str, value: Tracked) -> Tracked:
    expanded = value.approximate_interval()
    result = unary(name, value.real)
    scalar = mul_up(unary_lipschitz(name, expanded), value.error)
    derivative = unary_derivative(name, expanded)
    return Tracked(
        result,
        scalar,
        linear_scale(value.linear, derivative),
        mul_up(derivative.magnitude(), value.remainder),
    )


def multiply_tracked(left: Tracked, right: Tracked) -> Tracked:
    scalar = add_up(
        mul_up(left.real.magnitude(), right.error),
        mul_up(right.real.magnitude(), left.error),
        mul_up(left.error, right.error),
    )
    coefficients = linear_add(
        linear_scale(left.linear, right.real),
        linear_scale(right.linear, left.real),
    )
    remainder = add_up(
        mul_up(left.real.magnitude(), right.remainder),
        mul_up(right.real.magnitude(), left.remainder),
        mul_up(left.affine_radius(), right.affine_radius()),
    )
    return Tracked(
        imul(left.real, right.real), scalar, coefficients, remainder
    )


def evaluate_tracked(node: ast.AST, environment: dict[str, Tracked]) -> Tracked:
    if isinstance(node, ast.Expression):
        return evaluate_tracked(node.body, environment)
    if isinstance(node, ast.Name):
        try:
            return environment[node.id]
        except KeyError:
            raise Unresolved(f"unknown value {node.id}") from None
    if isinstance(node, ast.Constant) and isinstance(node.value, (int, float)):
        return Tracked(Interval.point(str(node.value)))
    if isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
        value = evaluate_tracked(node.operand, environment)
        return Tracked(
            ineg(value.real),
            value.error,
            linear_scale(value.linear, Interval.point(-1)),
            value.remainder,
        )
    if isinstance(node, ast.BinOp):
        if (
            isinstance(node.op, ast.Sub)
            and expression_key(node.left) == expression_key(node.right)
        ):
            return Tracked(Interval.point(0))
        left = evaluate_tracked(node.left, environment)
        right = evaluate_tracked(node.right, environment)
        if isinstance(node.op, ast.Add):
            return Tracked(
                iadd(left.real, right.real),
                add_up(left.error, right.error),
                linear_add(left.linear, right.linear),
                add_up(left.remainder, right.remainder),
            )
        if isinstance(node.op, ast.Sub):
            return Tracked(
                isub(left.real, right.real),
                add_up(left.error, right.error),
                linear_add(left.linear, right.linear, subtract=True),
                add_up(left.remainder, right.remainder),
            )
        if isinstance(node.op, ast.Mult):
            return multiply_tracked(left, right)
        if isinstance(node.op, ast.Div):
            approximate_denominator = right.approximate_interval()
            real_minimum = right.real.minimum_magnitude()
            approximate_minimum = approximate_denominator.minimum_magnitude()
            if real_minimum == 0:
                raise Unresolved("real denominator may contain zero")
            if approximate_minimum == 0:
                raise InvalidConfiguration(
                    "perturbed denominator may contain zero"
                )
            direct = up(lambda x, y: x / y, left.error, approximate_minimum)
            denominator = mul_up(real_minimum, approximate_minimum)
            reciprocal = up(
                lambda magnitude, error, divisor: magnitude * error / divisor,
                left.real.magnitude(),
                right.error,
                denominator,
            )
            inverse = unary_tracked("inv", right)
            result = multiply_tracked(left, inverse)
            # Preserve the independently derived emergency quotient enclosure.
            result.error = add_up(direct, reciprocal)
            return result
        if isinstance(node.op, ast.Pow):
            if right.bound() != 0 or right.real.lo != right.real.hi:
                raise Unresolved("only exact constant powers are certified")
            power_value = right.real.lo
            if not gmpy2.is_integer(power_value):
                raise Unresolved("only integer powers are certified")
            power = int(power_value)
            expanded = left.approximate_interval()
            if power < 0 and expanded.minimum_magnitude() == 0:
                raise Unresolved("negative power may divide by zero")
            if power == 0:
                propagated = gmpy2.mpfr(0)
            else:
                derivative = mul_up(
                    abs(power),
                    up(lambda x: x ** (abs(power) - 1), expanded.magnitude()),
                )
                if power < 0:
                    derivative = up(
                        lambda d, m, p: d / (m ** p),
                        abs(power),
                        expanded.minimum_magnitude(),
                        abs(power) + 1,
                    )
                propagated = mul_up(derivative, left.error)
            if power == 0:
                coefficients: dict[str, Interval] = {}
                remainder = gmpy2.mpfr(0)
            else:
                derivative_interval = imul(
                    Interval.point(power),
                    ipow(
                        expanded,
                        Interval.point(power - 1),
                    ),
                )
                coefficients = linear_scale(left.linear, derivative_interval)
                remainder = mul_up(
                    derivative_interval.magnitude(), left.remainder
                )
            return Tracked(
                ipow(left.real, right.real),
                propagated,
                coefficients,
                remainder,
            )
    if isinstance(node, ast.Call) and isinstance(node.func, ast.Name):
        name = node.func.id
        if (
            name == "mp"
            and len(node.args) == 1
            and isinstance(node.args[0], ast.Constant)
            and isinstance(node.args[0].value, str)
        ):
            return Tracked(Interval.point(node.args[0].value))
        if len(node.args) == 1:
            value = evaluate_tracked(node.args[0], environment)
            return unary_tracked(name, value)
        if name == "hypot" and len(node.args) == 2:
            left = evaluate_tracked(node.args[0], environment)
            right = evaluate_tracked(node.args[1], environment)
            squared = evaluate_tracked(
                ast.BinOp(
                    left=ast.BinOp(
                        left=node.args[0], op=ast.Mult(), right=node.args[0]
                    ),
                    op=ast.Add(),
                    right=ast.BinOp(
                        left=node.args[1], op=ast.Mult(), right=node.args[1]
                    ),
                ),
                environment,
            )
            return unary_tracked("sqrt", squared)
    raise Unresolved(f"unsupported expression node {ast.dump(node)}")


VARIABLE = re.compile(
    r"^\s*real\s+([A-Za-z_][A-Za-z0-9_]*)\s+in\s+\[\s*([^,]+),\s*([^\]]+)\]\s*;"
)


def preserve_decimal_literals(expression: str) -> str:
    """Wrap non-integer literals so Python never rounds them through float."""
    tokens = []
    for token in tokenize.generate_tokens(io.StringIO(expression).readline):
        if token.type == tokenize.NUMBER and any(
            marker in token.string.lower() for marker in (".", "e")
        ):
            token = tokenize.TokenInfo(
                token.type,
                f'mp("{token.string}")',
                token.start,
                token.end,
                token.line,
            )
        tokens.append(token)
    return tokenize.untokenize(tokens)
DEFINITION = re.compile(
    r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(?:rnd64|rnd\[[^\]]+\])\((.*)\);\s*$"
)


@functools.lru_cache(maxsize=128)
def parse_query(query: str) -> tuple[dict[str, Interval], list[tuple[str, ast.AST]], str]:
    variables: dict[str, Interval] = {}
    definitions: list[tuple[str, ast.AST]] = []
    section = ""
    root = ""
    for line in query.splitlines():
        stripped = line.strip()
        if stripped in {"Variables", "Definitions", "Expressions"}:
            section = stripped
            continue
        if not stripped:
            continue
        if section == "Variables":
            match = VARIABLE.match(line)
            if not match:
                raise Unresolved(f"unsupported domain declaration: {stripped}")
            variables[match.group(1)] = Interval(match.group(2), match.group(3))
        elif section == "Definitions":
            match = DEFINITION.match(line)
            if not match:
                raise Unresolved(f"unsupported definition: {stripped}")
            expression = preserve_decimal_literals(
                match.group(2).replace("^", "**")
            )
            definitions.append((match.group(1), ast.parse(expression, mode="eval")))
        elif section == "Expressions":
            root = stripped.rstrip(";")
    if not definitions or not root:
        raise Unresolved("query has no definitions or root expression")
    return variables, definitions, root


def qvalue(value: str) -> Any:
    if "/" in value:
        numerator, denominator = value.split("/", 1)
        return up(lambda a, b: a / b, gmpy2.mpfr(numerator), gmpy2.mpfr(denominator))
    return gmpy2.mpfr(value)


def candidate_argument(expression: ast.AST, operation: str) -> ast.AST | None:
    node = expression.body if isinstance(expression, ast.Expression) else expression
    if operation == "log1p":
        if (
            isinstance(node, ast.Call)
            and isinstance(node.func, ast.Name)
            and node.func.id == "log"
            and len(node.args) == 1
            and isinstance(node.args[0], ast.BinOp)
            and isinstance(node.args[0].op, ast.Add)
        ):
            addition = node.args[0]
            if isinstance(addition.left, ast.Constant) and addition.left.value == 1:
                return addition.right
            if isinstance(addition.right, ast.Constant) and addition.right.value == 1:
                return addition.left
    if operation == "expm1":
        if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Sub):
            node = node.left
    if isinstance(node, ast.Call) and len(node.args) >= 1:
        return node.args[0]
    return None


class ValidationBound:
    __slots__ = (
        "error",
        "scalar",
        "first_order",
        "remainder",
        "dominant_site",
    )

    def __init__(self, value: Tracked):
        self.scalar = gmpy2.next_above(value.error)
        self.first_order = gmpy2.next_above(linear_radius(value.linear))
        self.remainder = gmpy2.next_above(value.remainder)
        self.error = gmpy2.next_above(
            min(self.scalar, add_up(self.first_order, self.remainder))
        )
        self.dominant_site = ""
        if value.linear:
            self.dominant_site = max(
                value.linear.items(), key=lambda item: item[1].magnitude()
            )[0]


def validate_parsed(
    variables: dict[str, Interval],
    definitions: list[tuple[str, ast.AST]],
    root: str,
    noises: dict[str, Any],
) -> Any:
    environment = {name: Tracked(interval) for name, interval in variables.items()}
    symbolic_environment: dict[str, ast.AST] = {
        name: ast.Name(id=name, ctx=ast.Load()) for name in variables
    }
    for name, expression in definitions:
        specification = noises.get(name)
        if specification is not None and "domain_lower" in specification:
            operation = specification.get("operation", "")
            argument = candidate_argument(expression, operation)
            if argument is None:
                raise Unresolved(
                    f"cannot identify {operation} implementation-domain argument"
                )
            argument_interval = evaluate_tracked(
                argument, environment
            ).approximate_interval()
            lower = qvalue(specification["domain_lower"])
            upper = qvalue(specification["domain_upper"])
            if argument_interval.lo < lower or argument_interval.hi > upper:
                raise InvalidImplementationDomain(
                    f"{operation} perturbed input [{argument_interval.lo},"
                    f"{argument_interval.hi}] exceeds [{lower},{upper}]"
                )

        value = evaluate_tracked(expression, environment)
        expanded = expand_expression(expression, symbolic_environment)
        # Override the dependency-oblivious real interval with the safely
        # canonicalised expression range. Error propagation remains separate.
        value = Tracked(
            correlated_evaluate(expanded, variables),
            value.error,
            value.linear,
            value.remainder,
        )
        if specification is None:
            epsilon, delta = U, ETA
        else:
            epsilon = qvalue(specification["epsilon"])
            delta = qvalue(specification["delta"])
        radius = up(
            lambda e, magnitude, d: e * magnitude + d,
            epsilon,
            value.approximate_interval().magnitude(),
            delta,
        )
        value = value.add_noise(name, radius)
        if value.approximate_interval().magnitude() > MAX_FINITE:
            raise InvalidConfiguration(
                "selected implementation may overflow binary64"
            )
        environment[name] = value
        symbolic_environment[name] = expanded
    return ValidationBound(environment[root])


def split_variable(
    variables: dict[str, Interval], name: str
) -> tuple[dict[str, Interval], dict[str, Interval]] | None:
    interval = variables[name]
    if interval.hi <= interval.lo:
        return None
    midpoint = directed(
        gmpy2.RoundToNearest, lambda a, b: (a + b) / 2, interval.lo, interval.hi
    )
    if midpoint <= interval.lo or midpoint >= interval.hi:
        return None
    left = dict(variables)
    right = dict(variables)
    left[name] = Interval(interval.lo, midpoint)
    right[name] = Interval(midpoint, interval.hi)
    return left, right


def split_variables(
    variables: dict[str, Interval],
) -> tuple[dict[str, Interval], dict[str, Interval]] | None:
    choices = [
        (interval.hi - interval.lo, name)
        for name, interval in variables.items()
        if interval.hi > interval.lo
    ]
    if not choices:
        return None
    _, name = max(choices)
    return split_variable(variables, name)


def best_refinement_split(
    variables: dict[str, Interval],
    definitions: list[tuple[str, ast.AST]],
    root: str,
    noises: dict[str, Any],
) -> tuple[str, list[tuple[dict[str, Interval], ValidationBound]]] | None:
    """Choose the split with the tightest rigorously enclosed child maximum.

    Input width alone is a poor proxy for output-error sensitivity. In
    particular, narrow control variables can dominate cancellation-heavy
    expressions. Trial bounds are conservative and are reused as the accepted
    child bounds, so this selection adds no unsound pruning.
    """
    best: list[tuple[dict[str, Interval], ValidationBound]] | None = None
    best_name = ""
    best_error: Any | None = None
    for name, interval in variables.items():
        if interval.hi <= interval.lo:
            continue
        split = split_variable(variables, name)
        if split is None:
            continue
        try:
            children = [
                (
                    child,
                    validate_parsed(child, definitions, root, noises),
                )
                for child in split
            ]
        except (
            Unresolved,
            InvalidImplementationDomain,
            InvalidConfiguration,
            ValueError,
            OverflowError,
        ):
            continue
        child_error = max(bound.error for _, bound in children)
        if best_error is None or child_error < best_error:
            best = children
            best_name = name
            best_error = child_error
    return (best_name, best) if best is not None else None


def validate(
    entry: dict[str, Any], *, refinement_deadline: float | None = None
) -> dict[str, Any]:
    started = time.perf_counter()
    try:
        # Candidate queries differ only in rounding annotations; exact
        # epsilon/delta values arrive through ``noises``. Canonicalising the
        # annotation makes every assignment reuse the same expression DAG.
        canonical_query = re.sub(r"rnd\[[^]]+\]", "rnd64", entry["query"])
        variables, definitions, root = parse_query(canonical_query)
    except (Unresolved, ValueError, SyntaxError, OverflowError) as error:
        return {"status": "ambiguous", "error": "", "reason": str(error)}

    queue: list[tuple[dict[str, Interval], int]] = [(variables, 0)]
    certified: list[tuple[dict[str, Interval], ValidationBound]] = []
    subdivisions = 0
    last_error: Exception | None = None
    max_depth = int(entry.get("subdivision_depth", 8))
    max_regions = int(entry.get("max_regions", 64))
    while queue:
        box, depth = queue.pop()
        try:
            certified.append(
                (
                    box,
                    validate_parsed(
                        box, definitions, root, entry.get("noises", {})
                    ),
                )
            )
        except (
            Unresolved,
            InvalidImplementationDomain,
            InvalidConfiguration,
            ValueError,
            OverflowError,
        ) as error:
            last_error = error
            split = split_variables(box) if depth < max_depth else None
            if split is None or len(queue) + len(certified) + 2 > max_regions:
                status = (
                    "invalid_configuration"
                    if isinstance(
                        error,
                        (InvalidImplementationDomain, InvalidConfiguration),
                    )
                    else "ambiguous"
                )
                return {"status": status, "error": "", "reason": str(error)}
            queue.extend((child, depth + 1) for child in split)
            subdivisions += 1
    if not certified:
        return {
            "status": "ambiguous",
            "error": "",
            "reason": str(last_error or "no certified region"),
        }

    # A successful interval enclosure may still be unnecessarily loose.
    # Refine the region currently responsible for the global maximum instead
    # of uniformly doubling every region. Accepted children exactly cover
    # their parent, so this changes tightness and runtime but not soundness.
    if refinement_deadline is None:
        refinement_deadline = started + float(
            entry.get("refinement_budget_seconds", 0.05)
        )
    stagnant_levels = 0
    stopping_reason = "converged"
    refinement_levels = 0
    preferred_variable: str | None = None
    sensitivity_refinement = bool(entry.get("sensitivity_refinement", True))
    while len(certified) < max_regions:
        if time.perf_counter() >= refinement_deadline:
            stopping_reason = "time_budget"
            break
        previous_error = max(bound.error for _, bound in certified)
        level_target = min(max_regions, max(len(certified) + 1, len(certified) * 2))
        level_splittable = True
        while len(certified) < level_target:
            if time.perf_counter() >= refinement_deadline:
                stopping_reason = "time_budget"
                level_splittable = False
                break
            parent_index = max(
                range(len(certified)),
                key=lambda index: certified[index][1].error,
            )
            parent_box, parent_bound = certified[parent_index]
            if preferred_variable is None and sensitivity_refinement:
                selected = best_refinement_split(
                    parent_box,
                    definitions,
                    root,
                    entry.get("noises", {}),
                )
                if selected is None:
                    stopping_reason = "unsplittable"
                    level_splittable = False
                    break
                preferred_variable, children = selected
            else:
                split = (
                    split_variable(parent_box, preferred_variable)
                    if preferred_variable is not None
                    else split_variables(parent_box)
                )
                if split is None:
                    selected = best_refinement_split(
                        parent_box,
                        definitions,
                        root,
                        entry.get("noises", {}),
                    )
                    if selected is None:
                        stopping_reason = "unsplittable"
                        level_splittable = False
                        break
                    preferred_variable, children = selected
                else:
                    try:
                        children = [
                            (
                                child,
                                validate_parsed(
                                    child,
                                    definitions,
                                    root,
                                    entry.get("noises", {}),
                                ),
                            )
                            for child in split
                        ]
                    except (
                        Unresolved,
                        InvalidImplementationDomain,
                        InvalidConfiguration,
                        ValueError,
                        OverflowError,
                    ):
                        children = []
            if not children:
                stopping_reason = "unsplittable"
                level_splittable = False
                break
            if max(bound.error for _, bound in children) > parent_bound.error:
                stopping_reason = "nonmonotone_guard"
                level_splittable = False
                break
            certified[parent_index : parent_index + 1] = children
            subdivisions += 1
        child_error = max(bound.error for _, bound in certified)
        if not level_splittable:
            break
        improvement = (
            float((previous_error - child_error) / previous_error)
            if previous_error > 0
            else 0.0
        )
        refinement_levels += 1
        if improvement < float(entry.get("refinement_threshold", 0.02)):
            stagnant_levels += 1
            if stagnant_levels >= 2:
                stopping_reason = "converged"
                break
        else:
            stagnant_levels = 0
    else:
        stopping_reason = "region_limit"

    error = max(bound.error for _, bound in certified)
    scalar = max(bound.scalar for _, bound in certified)
    first_order = max(bound.first_order for _, bound in certified)
    remainder = max(bound.remainder for _, bound in certified)
    dominant = max(
        (bound for _, bound in certified),
        key=lambda bound: bound.first_order,
    ).dominant_site
    return {
        "status": "satire_certified",
        "error": format(error, ".70g"),
        "reason": f"regions={len(certified)},subdivisions={subdivisions}",
        "first_order": format(first_order, ".70g"),
        "nonlinear_remainder": format(remainder, ".70g"),
        "scalar_bound": format(scalar, ".70g"),
        "reported_bound": format(error, ".70g"),
        "regions": len(certified),
        "subdivisions": subdivisions,
        "refinement_levels": refinement_levels,
        "dominant_site": dominant,
        "stopping_reason": stopping_reason,
        "refinement_seconds": time.perf_counter() - started,
    }


def validate_with_budget(
    item: tuple[int, dict[str, Any], float]
) -> tuple[int, dict[str, Any]]:
    """Process-pool entry point for one independent candidate substitution."""
    index, entry, budget = item
    return (
        index,
        validate(
            entry,
            refinement_deadline=time.perf_counter() + max(0.0, budget),
        ),
    )


def process_manifest(manifest: dict[str, Any]) -> dict[str, Any]:
    if manifest.get("version") not in {1, 2, 3}:
        raise ValueError("unsupported SATIRE manifest version")
    unavailable = verify_satire_extension()
    if unavailable:
        results = [
            {"status": "ambiguous", "error": "", "reason": unavailable}
            for _ in manifest["queries"]
        ]
    else:
        queries = manifest["queries"]
        results: list[dict[str, Any] | None] = [None] * len(queries)

        def noise_score(entry: dict[str, Any]) -> float:
            total = 0.0
            for specification in entry.get("noises", {}).values():
                total += float(qvalue(specification["epsilon"]))
                total += float(qvalue(specification["delta"]))
            return total

        # Spend the shared refinement budget on the most accurate candidates
        # first; these are where fixed-rounding overestimation is visible.
        ordered = sorted(
            range(len(queries)), key=lambda i: noise_score(queries[i])
        )
        budget = float(manifest.get("refinement_budget_seconds", 0.5))
        workers = max(
            1,
            min(int(manifest.get("validation_jobs", 1)), len(ordered)),
        )
        if workers > 1 and len(ordered) > 1:
            # Candidate substitutions are independent once the canonical DAG
            # is available. Give every candidate a small useful refinement
            # slice, then reserve progressively deeper searches for the
            # low-noise accuracy anchors. Since these execute concurrently,
            # this improves the low-error frontier without multiplying wall
            # time by the candidate count.
            baseline_budget = min(0.02, budget)
            candidate_budgets = [baseline_budget] * len(ordered)
            for rank, share in enumerate((1.0, 0.5, 0.25, 0.125)):
                if rank < len(candidate_budgets):
                    candidate_budgets[rank] = max(
                        candidate_budgets[rank], budget * share
                    )
            work = []
            for position, index in enumerate(ordered):
                entry = dict(queries[index])
                # Sensitivity trials are valuable for the low-error anchors
                # but can exceed the tiny baseline slice assigned to other
                # candidates. Width splitting remains sound for those points.
                entry["sensitivity_refinement"] = position < 4
                work.append((index, entry, candidate_budgets[position]))
            with concurrent.futures.ProcessPoolExecutor(
                max_workers=workers
            ) as executor:
                for index, result in executor.map(validate_with_budget, work):
                    results[index] = result
        else:
            deadline = time.perf_counter() + budget
            for position, index in enumerate(ordered):
                # A single hard candidate must not consume the whole batch
                # budget. Divide remaining time fairly while retaining time
                # saved by earlier candidates.
                remaining_candidates = len(ordered) - position
                now = time.perf_counter()
                candidate_deadline = now + max(
                    0.0, (deadline - now) / remaining_candidates
                )
                results[index] = validate(
                    queries[index], refinement_deadline=candidate_deadline
                )
        results = [result for result in results if result is not None]
    return {
        "version": 3,
        "backend": "satire_affine_remainder",
        "satire_revision": manifest.get("satire_revision", "unknown"),
        "results": results,
    }


def serve(path: Path) -> int:
    path.unlink(missing_ok=True)
    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server.bind(str(path))
    server.listen()
    try:
        while True:
            connection, _ = server.accept()
            with connection, connection.makefile("rwb") as stream:
                line = stream.readline()
                request = json.loads(line)
                if request.get("shutdown"):
                    stream.write(b'{"status":"bye"}\n')
                    stream.flush()
                    return 0
                response = process_manifest(request)
                stream.write(
                    (json.dumps(response, separators=(",", ":")) + "\n").encode()
                )
                stream.flush()
    finally:
        server.close()
        path.unlink(missing_ok=True)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=Path)
    parser.add_argument("--output", type=Path)
    parser.add_argument("--server", type=Path)
    args = parser.parse_args()
    if args.server:
        return serve(args.server)
    if not args.input or not args.output:
        parser.error("--input and --output are required outside server mode")
    manifest = json.loads(args.input.read_text())
    payload = process_manifest(manifest)
    temporary = args.output.with_suffix(args.output.suffix + ".tmp")
    temporary.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
    os.replace(temporary, args.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
