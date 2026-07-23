from __future__ import annotations

import importlib.util
from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = ROOT / "src/scripts/satire_validate.py"
SPEC = importlib.util.spec_from_file_location("satire_validate", MODULE_PATH)
assert SPEC and SPEC.loader
SATIRE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(SATIRE)


def query(definitions: str, root: str = "v1") -> str:
    return f"""Variables
  real x in [1,2];

Definitions
{definitions}

Expressions
  {root};
"""


class SatireValidatorTests(unittest.TestCase):
    def test_arbitrary_epsilon_delta_is_certified(self) -> None:
        result = SATIRE.validate(
            {
                "query": query(
                    "  v0 = rnd64(x);\n"
                    "  v1 = rnd[64,ne,0.5,-10,-20](exp(v0));"
                ),
                "noises": {
                    "v1": {"epsilon": "1/1024", "delta": "1/1048576"}
                },
            }
        )
        self.assertEqual(result["status"], "satire_certified")
        self.assertGreater(float(result["error"]), 0.0)

    def test_cross_site_product_terms_are_included(self) -> None:
        result = SATIRE.validate(
            {
                "query": query(
                    "  v0 = rnd64(x);\n"
                    "  v1 = rnd[64,ne,0.5,-10,-20](v0 * v0);"
                ),
                "noises": {
                    "v0": {"epsilon": "1/1024", "delta": "0"},
                    "v1": {"epsilon": "1/1024", "delta": "0"},
                },
            }
        )
        self.assertEqual(result["status"], "satire_certified")
        # A first-order-only estimate would omit positive cross products.
        self.assertGreater(float(result["error"]), 3.0 / 1024.0)

    def test_unresolved_singularity_is_reported_without_a_false_bound(self) -> None:
        result = SATIRE.validate(
            {
                "query": """Variables
  real x in [-1,1];
Definitions
  v0 = rnd64(x);
  v1 = rnd64(inv(v0));
Expressions
  v1;
""",
                "noises": {},
            }
        )
        self.assertEqual(result["status"], "ambiguous")
        self.assertIn("zero", result["reason"])

    def test_shared_signed_noise_cancels(self) -> None:
        result = SATIRE.validate(
            {
                "query": query(
                    "  v0 = rnd64(x);\n"
                    "  v1 = rnd64(v0 - v0);"
                ),
                "noises": {},
            }
        )
        self.assertEqual(result["status"], "satire_certified")
        self.assertLess(float(result["error"]), 1e-300)

    def test_successful_refinement_tightens_exp1x(self) -> None:
        result = SATIRE.validate(
            {
                "query": """Variables
  real x in [0.0078125,0.5];
Definitions
  var_x_00 = rnd64(x);
  exp_00 = rnd64(exp(var_x_00));
  num_00 = rnd64(1);
  sub_00 = rnd64(exp_00 - num_00);
  var_x_01 = rnd64(x);
  div_00 = rnd64(sub_00 / var_x_01);
Expressions
  div_00;
""",
                "noises": {
                    "exp_00": {
                        "epsilon": "113/1000000000000000000",
                        "delta": "0",
                        "operation": "exp",
                        "domain_lower": "-1.7976931348623157e308",
                        "domain_upper": "709.782712893384",
                    }
                },
                "refinement_budget_seconds": 0.1,
            }
        )
        self.assertEqual(result["status"], "satire_certified")
        self.assertLess(float(result["error"]), 1e-13)
        self.assertLessEqual(
            float(result["error"]), float(result["scalar_bound"])
        )
        self.assertGreater(int(result["regions"]), 1)
        self.assertIn("first_order", result)
        self.assertIn("nonlinear_remainder", result)

    def test_correlated_repeated_factor_avoids_symmetric_overestimate(self) -> None:
        result = SATIRE.validate(
            {
                "query": """Variables
  real x in [-1,1];
  real y in [-5,5];
Definitions
  v0 = rnd64(x);
  v1 = rnd64(y);
  v2 = rnd64((v0 * v1) * v1);
  v3 = rnd64(exp(v2));
Expressions
  v3;
""",
                "noises": {},
            }
        )
        self.assertEqual(result["status"], "satire_certified")
        # Grouping y*y proves the exponent is at most 25 rather than 125.
        self.assertLess(float(result["error"]), 1.0)

    def test_candidate_that_can_make_denominator_zero_is_excluded(self) -> None:
        result = SATIRE.validate(
            {
                "query": """Variables
  real x in [0.0078125,0.5];
Definitions
  v0 = rnd64(x);
  v1 = rnd64(exp(v0));
  v2 = rnd64(log(v1));
  v3 = rnd64(inv(v2));
Expressions
  v3;
""",
                "noises": {
                    "v2": {"epsilon": "1/2", "delta": "1/64"},
                },
                "subdivision_depth": 4,
            }
        )
        self.assertEqual(result["status"], "invalid_configuration")
        self.assertIn("zero", result["reason"])

    def test_candidate_domain_is_checked_after_error_propagation(self) -> None:
        result = SATIRE.validate(
            {
                "query": query(
                    "  v0 = rnd64(x);\n"
                    "  v1 = rnd64(exp(v0));"
                ),
                "noises": {
                    "v1": {
                        "epsilon": "1/2",
                        "delta": "0",
                        "operation": "exp",
                        "domain_lower": "1",
                        "domain_upper": "2",
                    }
                },
                "subdivision_depth": 2,
            }
        )
        self.assertEqual(result["status"], "invalid_configuration")
        self.assertIn("exceeds", result["reason"])

    def test_refinement_chooses_sensitive_narrow_variable(self) -> None:
        variables = {
            "wide": SATIRE.Interval(
                SATIRE.gmpy2.mpfr("1"), SATIRE.gmpy2.mpfr("10")
            ),
            "sensitive": SATIRE.Interval(
                SATIRE.gmpy2.mpfr("0.00390625"),
                SATIRE.gmpy2.mpfr("0.125"),
            ),
        }
        _, definitions, root = SATIRE.parse_query(
            """Variables
  real wide in [1,10];
  real sensitive in [0.00390625,0.125];
Definitions
  v0 = rnd64(wide);
  v1 = rnd64(sensitive);
  v2 = rnd64(exp(v1));
  v3 = rnd64(v2 - 1);
  v4 = rnd64(v3 / v1);
Expressions
  v4;
"""
        )
        selected = SATIRE.best_refinement_split(
            variables, definitions, root, {}
        )
        self.assertIsNotNone(selected)
        assert selected is not None
        selected_name, children = selected
        self.assertEqual(selected_name, "sensitive")
        # The exponential cancellation is controlled by the narrow variable;
        # the selected split therefore leaves the wide interval untouched.
        self.assertTrue(
            all(
                child["wide"].lo == variables["wide"].lo
                and child["wide"].hi == variables["wide"].hi
                for child, _ in children
            )
        )


if __name__ == "__main__":
    unittest.main()
