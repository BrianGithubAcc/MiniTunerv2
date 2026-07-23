from __future__ import annotations

import importlib.util
from pathlib import Path
import unittest


ROOT = Path(__file__).resolve().parents[1]
MODULE_PATH = ROOT / "src/scripts/generate_benchmark_table.py"
SPEC = importlib.util.spec_from_file_location("generate_benchmark_table", MODULE_PATH)
assert SPEC and SPEC.loader
TABLE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(TABLE)


class BenchmarkTableTests(unittest.TestCase):
    def test_status_cells_report_time_or_explicit_failure(self) -> None:
        self.assertEqual(
            TABLE.status_cell(
                {"status": "ok", "complete": "True"},
                {"status": "ok", "core_seconds": "0.125"},
            ),
            "✅ 0.125 s",
        )
        self.assertEqual(
            TABLE.status_cell(
                {
                    "status": "unsupported",
                    "complete": "False",
                    "reason": "unsupported precision",
                },
                None,
            ),
            "❌ unsupported precision",
        )

    def test_elapsed_excludes_separate_plotting_time(self) -> None:
        self.assertAlmostEqual(
            TABLE.elapsed(
                {
                    "total_seconds": "4.5",
                    "plotting_seconds": "1.25",
                }
            ),
            3.25,
        )

    def test_status_and_time_are_separate_table_columns(self) -> None:
        self.assertEqual(
            TABLE.status_and_time(
                {"status": "ok", "complete": "True"},
                {"status": "ok", "tuner_seconds": "2.5"},
            ),
            ("✅", "2.500 s"),
        )
        self.assertEqual(
            TABLE.status_and_time(
                {"status": "timeout", "complete": "False", "reason": "timeout"},
                {"status": "timeout"},
            ),
            ("❌ timeout", "—"),
        )


if __name__ == "__main__":
    unittest.main()
