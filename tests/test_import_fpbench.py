import sys
import unittest
import math
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parents[1] / "src/scripts"))

from import_fpbench import (
    Unsupported,
    convert_expr,
    combine_interval,
    infer_binary64_domains,
    parse_forms,
    source_domain_info,
    source_domains,
    unpack_fpcore,
    rewrite_optuner_patterns,
)


class FPCoreImporterTests(unittest.TestCase):
    def test_sqrt_and_hypot_conversion(self):
        domains = {"x": ("1", "4"), "y": ("2", "3")}
        result = convert_expr(["hypot", "x", "y"], domains)
        self.assertIn("sqrt", result.text)
        self.assertIn("sqrt", result.operations)
        self.assertGreaterEqual(result.interval[0], 0.0)

    def test_negative_sqrt_is_rejected(self):
        with self.assertRaisesRegex(Unsupported, "sqrt argument may be negative"):
            convert_expr(["sqrt", "x"], {"x": ("-1", "4")})

    def test_variable_power_is_rejected(self):
        with self.assertRaisesRegex(Unsupported, "variable-exponent pow"):
            convert_expr(["pow", "x", "y"], {"x": ("1", "2"), "y": ("2", "3")})

    def test_atan2_positive_denominator_is_rewritten(self):
        result = convert_expr(["atan2", "y", "x"], {"x": ("1", "2"), "y": ("-1", "1")})
        self.assertIn("atan", result.text)

    def test_atan2_crossing_axis_is_rejected(self):
        with self.assertRaisesRegex(Unsupported, "atan2 domain crosses"):
            convert_expr(["atan2", "y", "x"], {"x": ("-1", "1"), "y": ("-1", "1")})

    def test_source_box_is_preserved(self):
        form = parse_forms('(FPCore (x) :name "s" :pre (<= 1 x 4) (sqrt x))')[0]
        arguments, properties, body = unpack_fpcore(form)
        self.assertEqual(source_domains(arguments, properties[":pre"]), {"x": ("1", "4")})
        self.assertEqual(body, ["sqrt", "x"])

    def test_relational_domain_uses_sound_enclosing_box(self):
        pre = [
            "and",
            ["<=", "0", "x", "2"],
            ["<", "-1", "y", "1"],
            ["<=", ["+", "x", "y"], "2"],
        ]
        domains, exact = source_domain_info(["x", "y"], pre)
        self.assertEqual(domains, {"x": ("0", "2"), "y": ("-1", "1")})
        self.assertFalse(exact)

    def test_square_interval_proves_positive_denominator(self):
        result = convert_expr(
            ["/", "x", ["+", ["*", "y", "y"], "1"]],
            {"x": ("-1", "1"), "y": ("-2", "2")},
        )
        self.assertLessEqual(result.interval[0], -1.0)
        self.assertGreaterEqual(result.interval[1], 1.0)

    def test_missing_sqrt_domain_is_inferred_nonnegative(self):
        domains = infer_binary64_domains(["x"], ["sqrt", "x"], {})
        self.assertEqual(domains["x"][0], "0")

    def test_missing_log_domain_is_inferred_positive(self):
        domains = infer_binary64_domains(["x"], ["log", "x"], {})
        self.assertGreater(float(domains["x"][0]), 0.0)

    def test_disconnected_missing_division_domain_is_not_guessed(self):
        with self.assertRaisesRegex(Unsupported, "disconnected at zero"):
            infer_binary64_domains(["x"], ["/", "1", "x"], {})

    def test_missing_exp_domain_avoids_overflow(self):
        domains = infer_binary64_domains(["x"], ["exp", "x"], {})
        self.assertLessEqual(float(domains["x"][1]), 709.782712893384)

    def test_interval_arithmetic_is_outward_rounded(self):
        low, high = combine_interval("+", (0.1, 0.1), (0.2, 0.2))
        rounded = 0.1 + 0.2
        self.assertLess(low, rounded)
        self.assertGreater(high, rounded)
        self.assertTrue(math.isfinite(low) and math.isfinite(high))

    def test_square_bracket_let_bindings_are_parsed_and_expanded(self):
        form = parse_forms(
            """
            (FPCore (x)
              :pre (<= 0 x 8)
              (let ([e (exp x)])
                (log (+ 1 e))))
            """
        )[0]
        arguments, properties, body = unpack_fpcore(form)
        domains = source_domains(arguments, properties[":pre"])
        converted = convert_expr(body, domains)
        self.assertIn("exp(x)", converted.text)
        self.assertIn("log", converted.text)
        self.assertEqual({"exp", "log"}, converted.operations)

    def test_optuner_rewrite_expands_let_and_recognizes_log1p(self):
        body = ["let", [["e", ["exp", "x"]]], ["log", ["+", "1", "e"]]]
        self.assertEqual(["log1p", ["exp", "x"]], rewrite_optuner_patterns(body))


if __name__ == "__main__":
    unittest.main()
