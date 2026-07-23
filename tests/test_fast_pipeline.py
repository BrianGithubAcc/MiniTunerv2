from __future__ import annotations

import importlib.util
from pathlib import Path
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = ROOT / "src/scripts/run_z3_bands.py"
SPEC = importlib.util.spec_from_file_location("run_z3_bands", MODULE_PATH)
assert SPEC and SPEC.loader
RUN_Z3_BANDS = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RUN_Z3_BANDS)


class FastPipelineTests(unittest.TestCase):
    def test_solver_build_check_detects_missing_and_newer_sources(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            executable = Path(temporary) / "minitune.exe"
            self.assertTrue(RUN_Z3_BANDS.solver_needs_build(executable))

    def test_fingerprint_separates_search_and_validation_backends(self) -> None:
        common = ("x in [0,1];sin(x)", "pareto")
        dp = RUN_Z3_BANDS.fingerprint(
            *common, "dp", "fptaylor", 4, "optuner_fptaylor"
        )
        z3 = RUN_Z3_BANDS.fingerprint(
            *common, "z3", "fptaylor", 4, "optuner_fptaylor"
        )
        hybrid = RUN_Z3_BANDS.fingerprint(
            *common, "dp", "satire-hybrid", 4, "optuner_satire"
        )
        self.assertNotEqual(dp, z3)
        self.assertNotEqual(dp, hybrid)

    def test_three_modes_are_exposed_by_all_runners(self) -> None:
        single = (ROOT / "run_minitune.sh").read_text()
        suite = (ROOT / "src/scripts/run_fpcore_suite.py").read_text()
        executable = (ROOT / "src/minitune.ml").read_text()
        for source in (single, suite, executable):
            self.assertIn("--optuner-compatible", source)
            self.assertIn("--optuner-satire-compatible", source)

    def test_satire_is_pinned_and_custom_noise_patch_is_checked_in(self) -> None:
        flake = (ROOT / "flake.nix").read_text()
        patch = (ROOT / "patches/satire-custom-noise.patch").read_text()
        self.assertIn("github:arnabd88/Satire/v1.1", flake)
        self.assertIn("set_custom_noise", patch)
        self.assertIn("SATIRE_PATH", flake)

    def test_satire_preprocessing_bypasses_external_analyzers(self) -> None:
        executable = (ROOT / "src/minitune.ml").read_text()
        solver = (ROOT / "src/z3_solver.ml").read_text()
        self.assertIn("FPTaylor/Gelpia calls=0", executable)
        self.assertIn("match fast_analysis with", solver)
        self.assertIn("Some analysis", solver)

    def test_fast_cache_tracks_both_satire_analyzers(self) -> None:
        runner = (ROOT / "src/scripts/run_z3_bands.py").read_text()
        self.assertIn('"src/satire_model.ml"', runner)
        self.assertIn('"src/scripts/satire_validate.py"', runner)

    def test_satire_mode_never_schedules_fptaylor_fallback(self) -> None:
        plotting = (ROOT / "src/plot_data.ml").read_text()
        hybrid_branch = plotting.split("let ambiguous =", 1)[1].split(
            "let fallback_started", 1
        )[0]
        self.assertNotIn("unresolved :=", hybrid_branch)
        self.assertIn('"validation_failed"', hybrid_branch)

    def test_satire_manifest_records_affine_refinement(self) -> None:
        plotting = (ROOT / "src/plot_data.ml").read_text()
        self.assertIn('("version", `Int 3)', plotting)
        self.assertIn("satire-hybrid-v3-affine-refined", plotting)
        self.assertIn("hybrid_first_order", plotting)
        self.assertIn("hybrid_remainder", plotting)

    def test_dp_frontier_uses_one_batched_z3_audit(self) -> None:
        solver = (ROOT / "src/z3_solver.ml").read_text()
        audit = solver.split("let audit_dp_frontier", 1)[1].split(
            "let exact_objective_point", 1
        )[0]
        self.assertEqual(audit.count("Z3.Solver.check solver []"), 1)
        self.assertIn("checks=%d, verified=%d", audit)

    def test_math_identifiers_allow_underscores(self) -> None:
        lexer = (ROOT / "src/math_parse/lexer.mll").read_text()
        self.assertIn("'_'", lexer)


if __name__ == "__main__":
    unittest.main()
