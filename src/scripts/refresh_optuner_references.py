#!/usr/bin/env python3
"""Freeze unchanged OpTuner structured results as compatibility fixtures."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import os
from pathlib import Path
import shutil
import tempfile


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def atomic_json(path: Path, value: object) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fd, temporary = tempfile.mkstemp(prefix=path.name + ".", dir=path.parent)
    try:
        with os.fdopen(fd, "w") as handle:
            json.dump(value, handle, indent=2, sort_keys=True)
            handle.write("\n")
        os.replace(temporary, path)
    except Exception:
        Path(temporary).unlink(missing_ok=True)
        raise


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--optuner-output", type=Path, required=True)
    parser.add_argument("--input-root", type=Path, required=True)
    parser.add_argument("--reference-root", type=Path, required=True)
    parser.add_argument("--optuner-root", type=Path, required=True)
    args = parser.parse_args()
    summary_path = args.optuner_output / "summary.csv"
    with summary_path.open(newline="") as handle:
        summary = list(csv.DictReader(handle))

    frontier_dir = args.reference_root / "frontiers"
    diagnostic_dir = args.reference_root / "diagnostics"
    frontier_dir.mkdir(parents=True, exist_ok=True)
    diagnostic_dir.mkdir(parents=True, exist_ok=True)
    cases = []
    for row in summary:
        if row.get("status") != "ok":
            continue
        slug = row["slug"]
        source_frontier = args.optuner_output / "frontiers" / f"{slug}.csv"
        if not source_frontier.exists():
            continue
        target_frontier = frontier_dir / f"{slug}.csv"
        shutil.copy2(source_frontier, target_frontier)
        source_diagnostic = args.optuner_output / "diagnostics" / f"{slug}.csv"
        diagnostic_name = ""
        if source_diagnostic.exists():
            target_diagnostic = diagnostic_dir / f"{slug}.csv"
            shutil.copy2(source_diagnostic, target_diagnostic)
            diagnostic_name = str(target_diagnostic.relative_to(args.reference_root))
        input_path = args.input_root / f"{slug}.txt"
        cases.append(
            {
                "index": int(row["index"]),
                "slug": slug,
                "benchmark_sha256": digest(input_path) if input_path.exists() else "",
                "frontier": str(target_frontier.relative_to(args.reference_root)),
                "frontier_sha256": digest(target_frontier),
                "diagnostics": diagnostic_name,
            }
        )

    implementation_files = [
        args.optuner_root / "implementations/all_costs.json",
        args.optuner_root / "implementations/all_specifications.json",
    ]
    fptaylor_config = args.optuner_root / "src/fptaylor/result.py"
    manifest = {
        "format": "minituner-optuner-reference-v1",
        "source": "unchanged OpTuner structured output",
        "cases": sorted(cases, key=lambda case: case["index"]),
        "implementation_data_sha256": {
            path.name: digest(path) for path in implementation_files
        },
        "fptaylor_configuration_sha256": digest(fptaylor_config),
    }
    atomic_json(args.reference_root / "manifest.json", manifest)
    print(f"Stored {len(cases)} OpTuner reference frontiers in {args.reference_root}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
