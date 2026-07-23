#!/usr/bin/env python3
"""Run exact MiniTuner cost bands concurrently and merge them soundly."""

from __future__ import annotations

import argparse
import concurrent.futures
import csv
from decimal import Decimal
import fcntl
from fractions import Fraction
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time
from typing import Any


ROOT = Path(__file__).resolve().parents[2]


def solver_needs_build(executable: Path) -> bool:
    if not executable.exists():
        return True
    executable_mtime = executable.stat().st_mtime_ns
    inputs = [ROOT / "src/dune"]
    inputs.extend((ROOT / "src").glob("*.ml"))
    for directory in (ROOT / "src/math_parse", ROOT / "src/taylor_parse"):
        inputs.extend(directory.glob("*.ml"))
        inputs.extend(directory.glob("*.mll"))
        inputs.extend(directory.glob("*.mly"))
    return any(path.exists() and path.stat().st_mtime_ns > executable_mtime for path in inputs)


def fingerprint(expression: str, engine: str, search_engine: str,
                validator: str, guidance_points: int,
                execution_mode: str) -> str:
    digest = hashlib.sha256()
    digest.update(b"z3-bands-v1\0")
    digest.update(expression.encode())
    digest.update(engine.encode())
    digest.update(search_engine.encode())
    digest.update(validator.encode())
    digest.update(str(guidance_points).encode())
    digest.update(execution_mode.encode())
    for path in (
        ROOT / "src/z3_solver.ml",
        ROOT / "src/minitune.ml",
        ROOT / "src/plot_data.ml",
        ROOT / "src/satire_model.ml",
        ROOT / "src/parse_input.ml",
        ROOT / "src/fptaylor.ml",
        ROOT / "src/scripts/run_z3_bands.py",
        ROOT / "src/scripts/satire_validate.py",
        ROOT / "implementations/all_costs.json",
        ROOT / "implementations/all_specifications_str.json",
    ):
        digest.update(path.read_bytes())
    return digest.hexdigest()


def choose_boundaries(costs: list[Decimal], bands: str, jobs: int) -> list[Decimal]:
    if not costs:
        return []
    requested = min(jobs, len(costs) + 1) if bands == "auto" else max(1, int(bands))
    boundary_count = requested - 1
    if boundary_count <= 0:
        return []
    if boundary_count >= len(costs):
        return costs
    indexes = {
        min(len(costs) - 1, max(0, round((index + 1) * len(costs) / (boundary_count + 1)) - 1))
        for index in range(boundary_count)
    }
    return [costs[index] for index in sorted(indexes)]


def decimal_text(value: Decimal) -> str:
    return format(value, "f")


def read_json(path: Path) -> dict[str, Any] | None:
    try:
        return json.loads(path.read_text())
    except (OSError, json.JSONDecodeError):
        return None


def run_band(
    *,
    band_id: int,
    low: Decimal | None,
    high: Decimal | None,
    args: argparse.Namespace,
    run_fingerprint: str,
    band_dir: Path,
) -> dict[str, Any]:
    name = f"band_{band_id:03d}"
    csv_path = band_dir / f"{name}.csv"
    metadata_path = band_dir / f"{name}.json"
    log_path = band_dir / f"{name}.log"
    expected = {
        "fingerprint": run_fingerprint,
        "band_id": band_id,
        "low": None if low is None else decimal_text(low),
        "high": None if high is None else decimal_text(high),
        "engine": args.z3_engine,
        "search_engine": args.search_engine,
        "validator": args.validator,
        "execution_mode": args.execution_mode,
        "site_semantics": args.site_semantics,
        "guidance_points": args.guidance_points,
    }
    existing = read_json(metadata_path) if args.resume else None
    if existing is not None and all(existing.get(key) == value for key, value in expected.items()):
        if existing.get("complete") is True and csv_path.exists():
            return existing

    base_command = [
        str(ROOT / "src/_build/default/minitune.exe"),
        "-e", args.expression,
        "-o", name,
        "--output-dir", str(band_dir),
        "--z3-engine", args.z3_engine,
        "--search-engine", args.search_engine,
        "--validator", args.validator,
        "--target-seconds", str(args.target_seconds),
        "--guidance-points", str(args.guidance_points),
        "--validation-jobs", str(args.validation_jobs),
        "--quiet",
    ]
    if args.optuner_compatible:
        base_command.append("--optuner-compatible")
    if args.optuner_satire_compatible:
        base_command.append("--optuner-satire-compatible")
    if low is not None:
        base_command += ["--z3-cost-low", decimal_text(low)]
    if high is not None:
        base_command += ["--z3-cost-high", decimal_text(high)]

    attempts = []
    complete = False
    started = time.monotonic()
    with log_path.open("w") as log:
        for candidate_points in [args.guidance_points]:
            command = list(base_command)
            attempt_start = time.monotonic()
            try:
                result = subprocess.run(
                    command,
                    cwd=ROOT / "src",
                    stdout=log,
                    stderr=subprocess.STDOUT,
                    timeout=args.timeout,
                    check=False,
                )
                status = f"exit_{result.returncode}"
                complete = result.returncode == 0 and csv_path.exists()
            except subprocess.TimeoutExpired:
                status = "timeout"
                complete = False
            attempts.append(
                {
                    "guidance_points": candidate_points,
                    "status": status,
                    "seconds": time.monotonic() - attempt_start,
                }
            )
            if complete:
                break

    log_text = log_path.read_text(errors="replace") if log_path.exists() else ""
    timing_match = re.search(
        r"Z3 enumeration: ([0-9.]+)s, engine=([^,]+), checks=(\d+), (\d+) Pareto models",
        log_text,
    )
    dp_match = re.search(
        r"Exact DP search: ([0-9.]+)s, sites=(\d+), (\d+) Pareto objective pairs",
        log_text,
    )
    audit_match = re.search(
        r"Z3 assignment audit: ([0-9.]+)s, checks=(\d+), verified=(\d+)",
        log_text,
    )
    size_match = re.search(
        r"Z3 candidates: (\d+) -> (\d+); choice Booleans: (\d+)", log_text
    )
    preprocessing_match = re.search(r"Exact preprocessing: ([0-9.]+)s", log_text)
    certification_match = re.search(
        r"Exact candidate certification: ([0-9.]+)s", log_text
    )
    validation_match = re.search(
        r"(?:FPTaylor|SATIRE) validation: ([0-9.]+)s", log_text
    )
    satire_match = re.search(
        r"SATIRE hybrid validation: ([0-9.]+)s, certified=(\d+), "
        r"ambiguous=(\d+), fallback=(\d+)",
        log_text,
    )
    fallback_match = re.search(
        r"FPTaylor fallback validation: ([0-9.]+)s", log_text
    )
    statistics = {
        "preprocessing_seconds": (
            float(preprocessing_match.group(1)) if preprocessing_match else 0.0
        ),
        "certification_seconds": (
            float(certification_match.group(1)) if certification_match else 0.0
        ),
        "z3_seconds": float(timing_match.group(1)) if timing_match else 0.0,
        "z3_checks": int(timing_match.group(3)) if timing_match else 0,
        "z3_models": int(timing_match.group(4)) if timing_match else 0,
        "dp_seconds": float(dp_match.group(1)) if dp_match else 0.0,
        "dp_pairs": int(dp_match.group(3)) if dp_match else 0,
        "z3_audit_seconds": float(audit_match.group(1)) if audit_match else 0.0,
        "z3_audit_checks": int(audit_match.group(2)) if audit_match else 0,
        "candidates_before": int(size_match.group(1)) if size_match else None,
        "candidates_after": int(size_match.group(2)) if size_match else None,
        "choice_booleans": int(size_match.group(3)) if size_match else None,
        "validation_seconds": (
            float(validation_match.group(1)) if validation_match else 0.0
        ),
        "satire_seconds": float(satire_match.group(1)) if satire_match else 0.0,
        "satire_certified": int(satire_match.group(2)) if satire_match else 0,
        "satire_ambiguous": int(satire_match.group(3)) if satire_match else 0,
        "fptaylor_fallback_count": (
            int(satire_match.group(4)) if satire_match else 0
        ),
        "fptaylor_fallback_seconds": (
            float(fallback_match.group(1)) if fallback_match else 0.0
        ),
    }
    if args.effective_validator == "satire_hybrid":
        statistics.update(
            {
                "satire_dag_seconds": statistics["preprocessing_seconds"],
                "optimizer_calls": 0,
                "optimizer_seconds": 0.0,
                "candidate_substitution_seconds": statistics["dp_seconds"],
            }
        )
    metadata = {
        **expected,
        **statistics,
        "complete": complete,
        "seconds": time.monotonic() - started,
        "attempts": attempts,
        "csv": str(csv_path),
        "log": str(log_path),
    }
    temporary = metadata_path.with_suffix(".json.tmp")
    temporary.write_text(json.dumps(metadata, indent=2, sort_keys=True) + "\n")
    os.replace(temporary, metadata_path)
    return metadata


def as_fraction(value: str) -> Fraction:
    return Fraction(value.strip())


def merge_bands(
    prefix: str, band_dir: Path, band_count: int, output_root: Path
) -> tuple[int, int]:
    rows: list[dict[str, str]] = []
    fields: list[str] = []
    diagnostic_rows: list[dict[str, str]] = []
    diagnostic_fields: list[str] = []
    for band_id in range(band_count):
        path = band_dir / f"band_{band_id:03d}.csv"
        with path.open(newline="") as source:
            reader = csv.DictReader(source)
            if reader.fieldnames:
                for field in reader.fieldnames:
                    if field not in fields:
                        fields.append(field)
            for row in reader:
                rows.append(row)

        diagnostic_path = band_dir / f"band_{band_id:03d}_diagnostics.csv"
        if diagnostic_path.exists():
            with diagnostic_path.open(newline="") as source:
                reader = csv.DictReader(source)
                if reader.fieldnames:
                    for field in reader.fieldnames:
                        if field not in diagnostic_fields:
                            diagnostic_fields.append(field)
                for row in reader:
                    row["band_id"] = str(band_id)
                    diagnostic_rows.append(row)

    def dominates(left: dict[str, str], right: dict[str, str]) -> bool:
        left_cost = as_fraction(left.get("cost_exact") or left["cost"])
        right_cost = as_fraction(right.get("cost_exact") or right["cost"])
        left_error = as_fraction(left.get("error_exact") or left["error"])
        right_error = as_fraction(right.get("error_exact") or right["error"])
        return (
            left_cost <= right_cost
            and left_error <= right_error
            and (left_cost < right_cost or left_error < right_error)
        )

    frontier = [row for row in rows if not any(dominates(other, row) for other in rows)]
    unique: dict[tuple[Fraction, Fraction], dict[str, str]] = {}
    for row in frontier:
        key = (
            as_fraction(row.get("cost_exact") or row["cost"]),
            as_fraction(row.get("error_exact") or row["error"]),
        )
        unique.setdefault(key, row)
    frontier = [unique[key] for key in sorted(unique)]

    frontier_keys = set(unique)
    for row in diagnostic_rows:
        if row.get("validation_status") not in {
            "certified",
            "satire_certified",
            "fptaylor_certified",
        }:
            continue
        diagnostic_error = row.get("error") or row.get("validated_error")
        if not diagnostic_error:
            continue
        key = (
            as_fraction(row.get("cost_exact") or row["cost"]),
            as_fraction(
                row.get("error_exact")
                or row.get("validated_error_exact")
                or diagnostic_error
            ),
        )
        if key not in frontier_keys:
            row["validation_status"] = "dominated"
    frontiers_dir = output_root / "frontiers"
    frontiers_dir.mkdir(parents=True, exist_ok=True)
    with (frontiers_dir / f"{prefix}_diagnostics.csv").open("w", newline="") as target:
        writer = csv.DictWriter(target, fieldnames=diagnostic_fields + ["band_id"])
        writer.writeheader()
        writer.writerows(diagnostic_rows)

    output = frontiers_dir / f"{prefix}.csv"
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open("w", newline="") as target:
        writer = csv.DictWriter(target, fieldnames=fields)
        writer.writeheader()
        for row in frontier:
            writer.writerow({field: row.get(field, "") for field in fields})
    return len(diagnostic_rows), len(frontier)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--expression", required=True)
    parser.add_argument("--prefix", required=True)
    parser.add_argument("--guidance-points", type=int, default=4)
    parser.add_argument("--output-root", type=Path, default=ROOT / "output/fpcore")
    parser.add_argument("--z3-engine", choices=("staircase", "pareto"), default="pareto")
    parser.add_argument("--search-engine", choices=("auto", "dp", "z3"), default="auto")
    parser.add_argument(
        "--validator", choices=("auto", "fptaylor", "satire-hybrid"), default="auto"
    )
    parser.add_argument("--target-seconds", type=float, default=10.0)
    parser.add_argument("--z3-jobs", type=int, default=min(3, os.cpu_count() or 1))
    parser.add_argument("--z3-bands", default="auto")
    parser.add_argument("--timeout", type=float, default=120.0)
    parser.add_argument("--resume", action=argparse.BooleanOptionalAction, default=True)
    parser.add_argument("--optuner-compatible", action="store_true")
    parser.add_argument("--optuner-satire-compatible", action="store_true")
    parser.add_argument("--validation-jobs", type=int, default=8)
    args = parser.parse_args()
    if args.optuner_compatible and args.optuner_satire_compatible:
        parser.error(
            "--optuner-compatible and --optuner-satire-compatible are mutually exclusive"
        )
    if (args.optuner_compatible or args.optuner_satire_compatible) and args.validator != "auto":
        parser.error("--validator is available only in native mode")
    if args.optuner_compatible:
        args.execution_mode = "optuner_fptaylor"
        args.site_semantics = "optuner_deduplicated"
        args.effective_validator = "fptaylor"
    elif args.optuner_satire_compatible:
        args.execution_mode = "optuner_satire"
        args.site_semantics = "optuner_deduplicated"
        args.effective_validator = "satire_hybrid"
    else:
        args.execution_mode = "native"
        args.site_semantics = "native_independent"
        args.effective_validator = (
            "satire_hybrid" if args.validator == "auto"
            else args.validator.replace("-", "_")
        )
    args.output_root = args.output_root.resolve()
    if args.guidance_points < 0:
        parser.error("--guidance-points must be nonnegative")
    if args.z3_jobs < 1:
        parser.error("--z3-jobs must be at least 1")
    if args.validation_jobs < 1:
        parser.error("--validation-jobs must be at least 1")
    if args.z3_bands != "auto" and int(args.z3_bands) < 1:
        parser.error("--z3-bands must be auto or a positive integer")

    costs: list[Decimal] = []
    boundaries = choose_boundaries(costs, args.z3_bands, args.z3_jobs)
    intervals = []
    for index in range(len(boundaries) + 1):
        intervals.append(
            (
                None if index == 0 else boundaries[index - 1],
                None if index == len(boundaries) else boundaries[index],
            )
        )

    band_dir = args.output_root / "checkpoints" / args.prefix
    band_dir.mkdir(parents=True, exist_ok=True)
    executable = ROOT / "src/_build/default/minitune.exe"
    if solver_needs_build(executable):
        # Multiple suite jobs can reach this point together. Serialize the
        # actual rebuild, then re-check after acquiring the lock.
        build_lock = args.output_root / "checkpoints/.dune-build.lock"
        build_lock.parent.mkdir(parents=True, exist_ok=True)
        with build_lock.open("w") as lock:
            fcntl.flock(lock.fileno(), fcntl.LOCK_EX)
            if solver_needs_build(executable):
                build = subprocess.run(
                    ["dune", "build", "./minitune.exe"],
                    cwd=ROOT / "src",
                    check=False,
                )
                if build.returncode != 0:
                    raise SystemExit("Failed to build the MiniTuner exact solver")
            fcntl.flock(lock.fileno(), fcntl.LOCK_UN)
    run_fingerprint = fingerprint(
        args.expression, args.z3_engine, args.search_engine, args.validator,
        args.guidance_points,
        args.execution_mode,
    )
    started = time.monotonic()
    results = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.z3_jobs) as executor:
        futures = [
            executor.submit(
                run_band,
                band_id=band_id,
                low=low,
                high=high,
                args=args,
                run_fingerprint=run_fingerprint,
                band_dir=band_dir,
            )
            for band_id, (low, high) in enumerate(intervals)
        ]
        for future in concurrent.futures.as_completed(futures):
            result = future.result()
            results.append(result)
            print(
                f"Z3 band {result['band_id']}: "
                f"{'complete' if result['complete'] else 'incomplete'} "
                f"in {result['seconds']:.3f}s"
            )

    complete = all(result["complete"] for result in results)
    summary: dict[str, Any] = {
        "complete": complete,
        "engine": args.z3_engine,
        "search_engine": args.search_engine,
        "validator": args.validator,
        "effective_validator": args.effective_validator,
        "execution_mode": args.execution_mode,
        "site_semantics": args.site_semantics,
        "guidance": "exact-candidates" if args.guidance_points else "none",
        "bands": len(intervals),
        "completed_bands": sum(bool(result["complete"]) for result in results),
        "seconds": time.monotonic() - started,
        "fingerprint": run_fingerprint,
        "z3_cpu_seconds": sum(result.get("z3_seconds") or 0.0 for result in results),
        "z3_checks": sum(result.get("z3_checks") or 0 for result in results),
        "z3_models": sum(result.get("z3_models") or 0 for result in results),
        "dp_seconds": sum(result.get("dp_seconds") or 0.0 for result in results),
        "dp_pairs": sum(result.get("dp_pairs") or 0 for result in results),
        "z3_audit_seconds": sum(
            result.get("z3_audit_seconds") or 0.0 for result in results
        ),
        "z3_audit_checks": sum(
            result.get("z3_audit_checks") or 0 for result in results
        ),
        "preprocessing_seconds": sum(
            result.get("preprocessing_seconds") or 0.0 for result in results
        ),
        "certification_seconds": sum(
            result.get("certification_seconds") or 0.0 for result in results
        ),
        "validation_seconds": sum(
            result.get("validation_seconds") or 0.0 for result in results
        ),
        "satire_seconds": sum(
            result.get("satire_seconds") or 0.0 for result in results
        ),
        "satire_dag_seconds": sum(
            result.get("satire_dag_seconds") or 0.0 for result in results
        ),
        "optimizer_calls": sum(
            result.get("optimizer_calls") or 0 for result in results
        ),
        "optimizer_seconds": sum(
            result.get("optimizer_seconds") or 0.0 for result in results
        ),
        "candidate_substitution_seconds": sum(
            result.get("candidate_substitution_seconds") or 0.0
            for result in results
        ),
        "satire_certified": sum(
            result.get("satire_certified") or 0 for result in results
        ),
        "satire_ambiguous": sum(
            result.get("satire_ambiguous") or 0 for result in results
        ),
        "fptaylor_fallback_count": sum(
            result.get("fptaylor_fallback_count") or 0 for result in results
        ),
        "fptaylor_fallback_seconds": sum(
            result.get("fptaylor_fallback_seconds") or 0.0 for result in results
        ),
    }
    if complete:
        models, frontier = merge_bands(
            args.prefix, band_dir, len(intervals), args.output_root
        )
        summary["models"] = models
        summary["frontier"] = frontier
        if (args.optuner_compatible or args.optuner_satire_compatible) and frontier == 0:
            complete = False
            summary["complete"] = False
            summary["reason"] = "no certified candidate solutions"
    summary_path = band_dir / "summary.json"
    summary_path.write_text(json.dumps(summary, indent=2, sort_keys=True) + "\n")
    print(json.dumps(summary, sort_keys=True))
    return 0 if complete else 1


if __name__ == "__main__":
    raise SystemExit(main())
