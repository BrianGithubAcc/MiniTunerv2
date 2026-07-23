#!/usr/bin/env python3
"""Compare MiniTuner compatibility output with frozen OpTuner frontiers."""

from __future__ import annotations

import argparse
import csv
from fractions import Fraction
import hashlib
import json
from pathlib import Path
import sys
from typing import Any


def file_hash(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def assignment(value: str) -> tuple[tuple[str, str], ...]:
    parsed = json.loads(value or "{}")
    return tuple(sorted((str(key), str(item)) for key, item in parsed.items()))


def rational(value: str) -> Fraction:
    return Fraction(value.strip())


def read_frontier(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as handle:
        return list(csv.DictReader(handle))


def point(row: dict[str, str]) -> tuple[Any, ...]:
    return (
        rational(row.get("cost_exact") or row["cost"]),
        rational(row.get("error_exact") or row["error"]),
        assignment(row.get("assignment", "")),
    )


def model_point(row: dict[str, str]) -> Fraction | None:
    value = row.get("model_error_exact") or row.get("model_error")
    return rational(value) if value else None


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--actual-root", type=Path, required=True)
    parser.add_argument("--reference-root", type=Path, required=True)
    parser.add_argument(
        "--check-model-error", action="store_true",
        help="Also require equality of the internal modeled-error diagnostic",
    )
    args = parser.parse_args()
    manifest_path = args.reference_root / "manifest.json"
    manifest = json.loads(manifest_path.read_text())
    cases = manifest.get("cases", [])
    rows: list[dict[str, Any]] = []
    failures = 0
    for case in cases:
        slug = case["slug"]
        reference = args.reference_root / case["frontier"]
        actual = args.actual_root / "frontiers" / f"{slug}.csv"
        status = "match"
        reason = ""
        if not actual.exists():
            status, reason = "missing", "MiniTuner frontier is missing"
        elif case.get("benchmark_sha256"):
            input_path = args.actual_root / "inputs" / f"{slug}.txt"
            if not input_path.exists() or file_hash(input_path) != case["benchmark_sha256"]:
                status, reason = "stale-benchmark", "benchmark hash differs from fixture"
        if status == "match":
            expected_rows = read_frontier(reference)
            actual_rows = read_frontier(actual)
            expected = [point(row) for row in expected_rows]
            observed = [point(row) for row in actual_rows]
            if observed != expected:
                status, reason = "frontier-mismatch", "cost, validated error, or assignment differs"
            elif args.check_model_error:
                expected_models = [model_point(row) for row in expected_rows]
                observed_models = [model_point(row) for row in actual_rows]
                if observed_models != expected_models:
                    status, reason = "model-error-mismatch", "modeled errors differ"
        if status != "match":
            failures += 1
        rows.append(
            {
                "index": case.get("index", ""),
                "slug": slug,
                "status": status,
                "reason": reason,
                "actual": str(actual),
                "reference": str(reference),
            }
        )

    report = args.actual_root / "compatibility_report.csv"
    with report.open("w", newline="") as handle:
        writer = csv.DictWriter(
            handle,
            fieldnames=("index", "slug", "status", "reason", "actual", "reference"),
        )
        writer.writeheader()
        writer.writerows(rows)
    summary = {
        "reference_manifest": str(manifest_path),
        "cases": len(rows),
        "matches": len(rows) - failures,
        "failures": failures,
        "model_error_checked": args.check_model_error,
        "report": str(report),
    }
    (args.actual_root / "compatibility_report.json").write_text(
        json.dumps(summary, indent=2, sort_keys=True) + "\n"
    )
    print(json.dumps(summary, indent=2, sort_keys=True))
    return 0 if failures == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
