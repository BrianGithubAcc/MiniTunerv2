#!/usr/bin/env python3
"""Convert the supported, finite-box subset of an FPCore suite to MiniTuner inputs."""

from __future__ import annotations

import argparse
import csv
import json
import math
import re
import time
from dataclasses import dataclass
from fractions import Fraction
from pathlib import Path
from typing import Any

TUNABLE = {"sin", "cos", "tan", "exp", "log", "log1p", "expm1", "sqrt"}
PI = "3.141592653589793238462643383279502884"
MAX_FINITE = "1.7976931348623157e308"
MIN_POSITIVE = "4.9406564584124654e-324"


class Unsupported(ValueError):
    pass


def outward(interval: tuple[float, float]) -> tuple[float, float]:
    """Expand a computed interval by one binary64 step in each direction."""
    low, high = interval
    if not math.isfinite(low) or not math.isfinite(high):
        raise Unsupported("arithmetic may overflow binary64")
    expanded = (math.nextafter(low, -math.inf), math.nextafter(high, math.inf))
    if not all(math.isfinite(value) for value in expanded):
        raise Unsupported("arithmetic may overflow binary64")
    return expanded


def outward_nonnegative(interval: tuple[float, float]) -> tuple[float, float]:
    low, high = outward(interval)
    return (max(0.0, low), high)


def tokenize(text: str) -> list[str]:
    tokens: list[str] = []
    i = 0
    while i < len(text):
        if text[i].isspace():
            i += 1
        elif text[i] == ";":
            end = text.find("\n", i)
            i = len(text) if end < 0 else end + 1
        elif text[i] in "()[]":
            tokens.append("(" if text[i] == "[" else ")" if text[i] == "]" else text[i])
            i += 1
        elif text[i] == '"':
            j, escaped = i + 1, False
            while j < len(text):
                if text[j] == '"' and not escaped:
                    break
                escaped = text[j] == "\\" and not escaped
                if text[j] != "\\":
                    escaped = False
                j += 1
            if j >= len(text):
                raise ValueError("unterminated string")
            tokens.append(text[i : j + 1])
            i = j + 1
        else:
            j = i
            while j < len(text) and not text[j].isspace() and text[j] not in "()[];":
                j += 1
            tokens.append(text[i:j])
            i = j
    return tokens


def parse_forms(text: str) -> list[Any]:
    tokens = tokenize(text)
    position = 0

    def parse_one() -> Any:
        nonlocal position
        if position >= len(tokens):
            raise ValueError("unexpected end of input")
        token = tokens[position]
        position += 1
        if token != "(":
            if token == ")":
                raise ValueError("unexpected closing parenthesis")
            return json.loads(token) if token.startswith('"') else token
        result = []
        while position < len(tokens) and tokens[position] != ")":
            result.append(parse_one())
        if position >= len(tokens):
            raise ValueError("unterminated list")
        position += 1
        return result

    forms = []
    while position < len(tokens):
        forms.append(parse_one())
    return forms


def number(value: str) -> float:
    if value == "PI":
        return math.pi
    return float(Fraction(value))


def is_number(value: Any) -> bool:
    if not isinstance(value, str):
        return False
    try:
        number(value)
        return True
    except (ValueError, ZeroDivisionError):
        return False


def slugify(name: str) -> str:
    slug = re.sub(r"[^a-z0-9]+", "_", name.lower()).strip("_")
    return slug or "unnamed"


def flatten_and(expr: Any) -> list[Any]:
    if isinstance(expr, list) and expr and expr[0] == "and":
        result: list[Any] = []
        for child in expr[1:]:
            result.extend(flatten_and(child))
        return result
    return [expr]


def source_domain_info(
    arguments: list[str], precondition: Any
) -> tuple[dict[str, tuple[str, str]], bool]:
    """Return a finite box and whether it exactly represents the precondition.

    Strict endpoints are closed conservatively.  Additional relational clauses
    are retained only as provenance: analysing the enclosing box is sound for
    an error upper bound, but may be less precise than analysing the relation.
    """
    bounds: dict[str, list[str | None]] = {
        argument: [None, None] for argument in arguments
    }
    exact_box = True
    if precondition is None:
        return {}, True
    for clause in flatten_and(precondition):
        matched = False
        if isinstance(clause, list) and clause and clause[0] in {"<", "<="}:
            if (
                len(clause) == 4
                and is_number(clause[1])
                and clause[2] in bounds
                and is_number(clause[3])
            ):
                bounds[clause[2]] = [clause[1], clause[3]]
                matched = True
            elif len(clause) == 3:
                left, right = clause[1], clause[2]
                if isinstance(left, str) and left in bounds and is_number(right):
                    bounds[left][1] = right
                    matched = True
                elif is_number(left) and isinstance(right, str) and right in bounds:
                    bounds[right][0] = left
                    matched = True
            if matched and clause[0] == "<":
                exact_box = False
        elif isinstance(clause, list) and len(clause) == 3 and clause[0] in {">", ">="}:
            left, right = clause[1], clause[2]
            if isinstance(left, str) and left in bounds and is_number(right):
                bounds[left][0] = right
                matched = True
            elif is_number(left) and isinstance(right, str) and right in bounds:
                bounds[right][1] = left
                matched = True
            if matched and clause[0] == ">":
                exact_box = False
        if not matched:
            exact_box = False

    domains = {
        argument: (values[0], values[1])
        for argument, values in bounds.items()
        if values[0] is not None and values[1] is not None
    }
    return domains, exact_box


def source_domains(arguments: list[str], precondition: Any) -> dict[str, tuple[str, str]]:
    domains, _ = source_domain_info(arguments, precondition)
    return domains


@dataclass
class Converted:
    text: str
    interval: tuple[float, float]
    operations: set[str]


def combine_interval(op: str, left: tuple[float, float], right: tuple[float, float]) -> tuple[float, float]:
    a, b = left
    c, d = right
    if op == "+":
        result = (a + c, b + d)
    elif op == "-":
        result = (a - d, b - c)
    elif op == "*":
        values = (a * c, a * d, b * c, b * d)
        result = (min(values), max(values))
    elif op == "/":
        if c <= 0.0 <= d:
            raise Unsupported("division domain includes zero")
        values = (a / c, a / d, b / c, b / d)
        result = (min(values), max(values))
    else:
        raise AssertionError(op)
    return outward(result)


def infer_binary64_domains(
    arguments: list[str], body: Any, existing: dict[str, tuple[str, str]]
) -> dict[str, tuple[str, str]]:
    """Infer one complete, operation-valid binary64 box when it is provable.

    The inference is intentionally conservative. Disconnected or periodic
    domains are reported as unresolved instead of silently selecting a subset.
    """
    domains = dict(existing)
    inferred = {argument for argument in arguments if argument not in domains}
    for argument in inferred:
        domains[argument] = (f"-{MAX_FINITE}", MAX_FINITE)

    def restrict(variable: Any, low: str | None = None, high: str | None = None) -> None:
        if not isinstance(variable, str) or variable not in inferred:
            return
        current_low, current_high = domains[variable]
        if low is not None and number(low) > number(current_low):
            current_low = low
        if high is not None and number(high) < number(current_high):
            current_high = high
        domains[variable] = (current_low, current_high)

    def visit(expr: Any) -> None:
        if not isinstance(expr, list) or not expr:
            return
        op = expr[0]
        if not isinstance(op, str):
            for child in expr:
                visit(child)
            return
        if op == "sqrt" and len(expr) == 2:
            restrict(expr[1], low="0")
        elif op == "log" and len(expr) == 2:
            restrict(expr[1], low=MIN_POSITIVE)
        elif op == "log1p" and len(expr) == 2:
            restrict(expr[1], low="-0.9999999999999999")
        elif op in {"exp", "expm1"} and len(expr) == 2:
            restrict(expr[1], high="709.782712893384")
        elif op == "tan" and len(expr) == 2 and isinstance(expr[1], str) and expr[1] in inferred:
            raise Unsupported("valid tan domain is disconnected across poles")
        elif op == "/" and len(expr) == 3 and isinstance(expr[2], str) and expr[2] in inferred:
            raise Unsupported("valid division domain is disconnected at zero")
        elif op == "pow" and len(expr) == 3 and is_number(expr[2]):
            exponent = Fraction(expr[2])
            if exponent.denominator == 1 and exponent.numerator < 0 and isinstance(expr[1], str) and expr[1] in inferred:
                raise Unsupported("valid negative-power domain is disconnected at zero")
        for child in expr[1:]:
            visit(child)

    visit(body)
    for variable, (low, high) in domains.items():
        if number(low) > number(high):
            raise Unsupported(f"empty inferred binary64 domain for {variable}")
    return domains


def convert_expr(expr: Any, domains: dict[str, tuple[str, str]], bindings: dict[str, Any] | None = None) -> Converted:
    bindings = {} if bindings is None else bindings
    if isinstance(expr, str):
        if expr in bindings:
            return convert_expr(bindings[expr], domains, bindings)
        if expr == "PI":
            return Converted(PI, (math.pi, math.pi), set())
        if is_number(expr):
            value = number(expr)
            return Converted(expr, (value, value), set())
        if expr not in domains:
            raise Unsupported(f"unbounded variable: {expr}")
        low, high = domains[expr]
        return Converted(expr, (number(low), number(high)), set())
    if not isinstance(expr, list) or not expr:
        raise Unsupported("malformed expression")

    op = expr[0]
    if op in {"if", "while", "while*", "for", "for*", "tensor", "tensor*"}:
        raise Unsupported("loops or conditionals")
    if op in {"let", "let*"}:
        if len(expr) != 3 or not isinstance(expr[1], list):
            raise Unsupported("malformed let expression")
        local = dict(bindings)
        for binding in expr[1]:
            if not isinstance(binding, list) or len(binding) != 2:
                raise Unsupported("malformed let binding")
            local[binding[0]] = binding[1]
        return convert_expr(expr[2], domains, local)
    if op == "!":
        return convert_expr(expr[-1], domains, bindings)

    if op in {"+", "*", "-", "/"}:
        if op == "-" and len(expr) == 2:
            child = convert_expr(expr[1], domains, bindings)
            return Converted(f"(-({child.text}))", (-child.interval[1], -child.interval[0]), child.operations)
        if len(expr) < 3 or op == "/" and len(expr) != 3:
            raise Unsupported(f"unsupported arity for {op}")
        children = [convert_expr(child, domains, bindings) for child in expr[1:]]
        if op == "*" and len(expr) == 3 and expr[1] == expr[2]:
            child = children[0]
            low, high = child.interval
            square_interval = outward_nonnegative((
                0.0 if low <= 0.0 <= high else min(low * low, high * high),
                max(low * low, high * high),
            ))
            return Converted(
                f"({child.text} * {child.text})", square_interval, child.operations
            )
        result = children[0]
        for child in children[1:]:
            result = Converted(
                f"({result.text} {op} {child.text})",
                combine_interval(op, result.interval, child.interval),
                result.operations | child.operations,
            )
        return result

    if op == "pow":
        if len(expr) != 3 or not is_number(expr[2]):
            raise Unsupported("variable-exponent pow")
        base = convert_expr(expr[1], domains, bindings)
        exponent = Fraction(expr[2])
        if exponent.denominator != 1:
            raise Unsupported("non-integer pow exponent")
        n = exponent.numerator
        if n < 0 and base.interval[0] <= 0.0 <= base.interval[1]:
            raise Unsupported("negative power domain includes zero")
        candidates = [base.interval[0] ** n, base.interval[1] ** n]
        if n % 2 == 0 and base.interval[0] <= 0 <= base.interval[1]:
            candidates.append(0.0)
        interval = outward((min(candidates), max(candidates)))
        if n >= 0 and n % 2 == 0:
            interval = (max(0.0, interval[0]), interval[1])
        return Converted(f"({base.text} ^ {n})", interval, base.operations)

    if op == "hypot" and len(expr) == 3:
        left = convert_expr(expr[1], domains, bindings)
        right = convert_expr(expr[2], domains, bindings)
        rewritten = ["sqrt", ["+", ["*", expr[1], expr[1]], ["*", expr[2], expr[2]]]]
        result = convert_expr(rewritten, domains, bindings)
        result.operations.update(left.operations | right.operations)
        return result

    if op == "atan2" and len(expr) == 3:
        y = convert_expr(expr[1], domains, bindings)
        x = convert_expr(expr[2], domains, bindings)
        if x.interval[0] > 0:
            suffix = ""
        elif x.interval[1] < 0 and y.interval[0] >= 0:
            suffix = f" + {PI}"
        elif x.interval[1] < 0 and y.interval[1] <= 0:
            suffix = f" - {PI}"
        else:
            raise Unsupported("atan2 domain crosses a branch or axis")
        ratio = combine_interval("/", y.interval, x.interval)
        return Converted(f"(atan(({y.text}) / ({x.text})){suffix})", (-math.pi, math.pi), y.operations | x.operations | {"atan"})

    if op in {"sqrt", "fabs", "abs", "atan", "sin", "cos", "tan", "exp", "log", "log1p", "expm1"} and len(expr) == 2:
        child = convert_expr(expr[1], domains, bindings)
        low, high = child.interval
        rendered = "abs" if op == "fabs" else op
        if op == "sqrt":
            if low < 0:
                raise Unsupported("sqrt argument may be negative")
            interval = outward_nonnegative((math.sqrt(low), math.sqrt(high)))
        elif op in {"log", "log1p"}:
            threshold = 0.0 if op == "log" else -1.0
            if low <= threshold:
                raise Unsupported(f"{op} argument crosses its domain boundary")
            fn = math.log if op == "log" else math.log1p
            interval = outward((fn(low), fn(high)))
        elif op == "exp" or op == "expm1":
            fn = math.exp if op == "exp" else math.expm1
            try:
                interval = outward((fn(low), fn(high)))
                if op == "exp":
                    interval = (max(0.0, interval[0]), interval[1])
            except OverflowError as error:
                raise Unsupported(f"{op} argument may overflow") from error
        elif op in {"abs", "fabs"}:
            interval = (0.0 if low <= 0 <= high else min(abs(low), abs(high)), max(abs(low), abs(high)))
        elif op == "atan":
            interval = outward((math.atan(low), math.atan(high)))
        elif op in {"sin", "cos"}:
            interval = (-1.0, 1.0)
        else:
            first = math.floor((low - math.pi / 2) / math.pi)
            last = math.floor((high - math.pi / 2) / math.pi)
            if first != last or abs(math.cos(low)) < 1e-15 or abs(math.cos(high)) < 1e-15:
                raise Unsupported("tan argument may cross a pole")
            values = (math.tan(low), math.tan(high))
            interval = outward((min(values), max(values)))
        return Converted(f"{rendered}({child.text})", interval, child.operations | {rendered})

    raise Unsupported(f"unsupported operation: {op}")


def rewrite_optuner_patterns(expr: Any, bindings: dict[str, Any] | None = None) -> Any:
    """Apply the source rewrites performed by OpTuner before naming sites.

    Let bindings are expanded first so syntactically equivalent direct and
    let-bound inputs receive the same canonical operation tree.
    """
    bindings = {} if bindings is None else bindings
    if isinstance(expr, str):
        return rewrite_optuner_patterns(bindings[expr], bindings) if expr in bindings else expr
    if not isinstance(expr, list) or not expr:
        return expr
    op = expr[0]
    if op in {"let", "let*"} and len(expr) == 3 and isinstance(expr[1], list):
        local = dict(bindings)
        for binding in expr[1]:
            if not isinstance(binding, list) or len(binding) != 2:
                raise Unsupported("malformed let binding")
            local[binding[0]] = binding[1]
        return rewrite_optuner_patterns(expr[2], local)
    rewritten = [op] + [rewrite_optuner_patterns(child, bindings) for child in expr[1:]]
    if (
        op == "log"
        and len(rewritten) == 2
        and isinstance(rewritten[1], list)
        and len(rewritten[1]) == 3
        and rewritten[1][0] == "+"
        and rewritten[1][1] in {"1", "1.", "1.0"}
    ):
        return ["log1p", rewritten[1][2]]
    return rewritten


def unpack_fpcore(form: Any) -> tuple[list[str], dict[str, Any], Any]:
    if not isinstance(form, list) or len(form) < 3 or form[0] != "FPCore":
        raise Unsupported("not an FPCore form")
    index = 1
    if isinstance(form[index], str):  # optional identifier
        index += 1
    arguments = form[index]
    index += 1
    properties: dict[str, Any] = {}
    while index + 1 < len(form) and isinstance(form[index], str) and form[index].startswith(":"):
        properties[form[index]] = form[index + 1]
        index += 2
    if index != len(form) - 1:
        raise Unsupported("malformed FPCore properties")
    return arguments, properties, form[index]


def normalize_arguments(arguments: list[Any]) -> list[str]:
    result: list[str] = []
    for argument in arguments:
        if isinstance(argument, str):
            result.append(argument)
            continue
        if isinstance(argument, list) and argument and argument[0] == "!":
            precision = None
            for index in range(1, len(argument) - 1, 2):
                if argument[index] == ":precision":
                    precision = argument[index + 1]
            if precision != "binary64":
                raise Unsupported(f"unsupported argument precision: {precision or 'unknown'}")
            if isinstance(argument[-1], str):
                result.append(argument[-1])
                continue
        raise Unsupported("malformed FPCore argument")
    return result


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", default="benchmarks/fpcore.json")
    parser.add_argument("--overrides", default="benchmarks/domain_overrides.json")
    parser.add_argument("--output-dir", default="output/fpcore/inputs")
    parser.add_argument("--optuner-compatible", action="store_true")
    args = parser.parse_args()

    source = Path(args.source)
    output = Path(args.output_dir)
    output.mkdir(parents=True, exist_ok=True)
    overrides = json.loads(Path(args.overrides).read_text()) if Path(args.overrides).exists() else {}
    records: list[dict[str, Any]] = []
    used_slugs: dict[str, int] = {}

    for index, form in enumerate(parse_forms(source.read_text()), start=1):
        benchmark_start = time.perf_counter()
        domain_inference_seconds = 0.0
        name = f"benchmark_{index}"
        provenance = "source"
        try:
            arguments, properties, body = unpack_fpcore(form)
            if args.optuner_compatible:
                body = rewrite_optuner_patterns(body)
            arguments = normalize_arguments(arguments)
            name = properties.get(":name", name)
            precision = properties.get(":precision", "binary64")
            if precision != "binary64":
                raise Unsupported(f"unsupported precision: {precision}")
            domains, exact_box = source_domain_info(arguments, properties.get(":pre"))
            if domains and not exact_box:
                provenance = "source_box_overapprox"
            if len(domains) != len(arguments):
                override = overrides.get(name)
                if override:
                    domains = {key: tuple(value) for key, value in override["domains"].items()}
                    provenance = override["provenance"]
                else:
                    inference_start = time.perf_counter()
                    domains = infer_binary64_domains(arguments, body, domains)
                    domain_inference_seconds = time.perf_counter() - inference_start
                    provenance = "inferred_binary64_valid"
            if set(domains) != set(arguments):
                raise Unsupported("missing safe domain")
            converted = convert_expr(body, domains)
            mode = "tunable" if converted.operations & TUNABLE else "fixed"
            base_slug = slugify(name)
            count = used_slugs.get(base_slug, 0)
            used_slugs[base_slug] = count + 1
            slug = base_slug if count == 0 else f"{base_slug}_{count + 1}"
            domain_text = ";".join(f"{var} in [{domains[var][0]},{domains[var][1]}]" for var in arguments)
            problem = f"{domain_text};{converted.text}"
            path = output / f"{slug}.txt"
            path.write_text(problem + "\n")
            records.append({"index": index, "name": name, "slug": slug, "status": "ready", "reason": "", "domain_provenance": provenance, "mode": mode, "region_count": 1, "import_seconds": time.perf_counter() - benchmark_start, "domain_inference_seconds": domain_inference_seconds, "input": str(path), "expression": problem})
        except (Unsupported, ValueError, OverflowError) as error:
            records.append({"index": index, "name": name, "slug": "", "status": "skipped", "reason": str(error), "domain_provenance": provenance, "mode": "", "region_count": 0, "import_seconds": time.perf_counter() - benchmark_start, "domain_inference_seconds": domain_inference_seconds, "input": "", "expression": ""})

    (output / "manifest.json").write_text(json.dumps(records, indent=2) + "\n")
    with (output / "summary.csv").open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=["index", "name", "slug", "status", "reason", "domain_provenance", "mode", "region_count", "import_seconds", "domain_inference_seconds", "input"])
        writer.writeheader()
        for record in records:
            writer.writerow({key: record[key] for key in writer.fieldnames})
    ready = sum(record["status"] == "ready" for record in records)
    print(f"Imported {ready}/{len(records)} benchmarks; manifest: {output / 'manifest.json'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
