#!/usr/bin/env python3
"""Import, run, and report every FPCore benchmark."""

from __future__ import annotations

import argparse
import concurrent.futures
import csv
import json
import os
from pathlib import Path
import re
import shutil
import socket
import subprocess
import sys
import time
from typing import Any

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from plot import plot_frontier

ROOT = Path(__file__).resolve().parents[2]


def status_image(path: Path, name: str, status: str, reason: str) -> float:
    started = time.perf_counter()
    path.parent.mkdir(parents=True, exist_ok=True)
    figure, axis = plt.subplots(figsize=(8, 4.5))
    axis.axis("off")
    axis.text(0.5, 0.72, name, ha="center", va="center", fontsize=16, weight="bold")
    axis.text(0.5, 0.52, status.upper(), ha="center", va="center", fontsize=14, color="darkred")
    axis.text(0.5, 0.30, reason, ha="center", va="center", fontsize=10, wrap=True)
    figure.tight_layout()
    figure.savefig(path, dpi=180)
    plt.close(figure)
    return time.perf_counter() - started


def unsupported_status(reason: str) -> str:
    lowered = reason.lower()
    if "precision" in lowered:
        return "unsupported-precision"
    if "loops or conditionals" in lowered:
        return "unsupported-control-flow"
    if any(word in lowered for word in ("domain", "overflow", "unbounded", "pole")):
        return "domain-inference"
    return "unsupported"


def extract_seconds(pattern: str, text: str) -> float:
    match = re.search(pattern, text)
    return float(match.group(1)) if match else 0.0


def csv_has_data(path: Path) -> bool:
    try:
        with path.open(newline="") as handle:
            reader = csv.reader(handle)
            next(reader)
            return next(reader, None) is not None
    except (OSError, StopIteration):
        return False


def run_one(row: dict[str, Any], args: argparse.Namespace) -> dict[str, Any]:
    output_root: Path = args.output_root
    slug = row["slug"]
    log_path = output_root / "logs" / f"{slug}.log"
    frontier = output_root / "frontiers" / f"{slug}.csv"
    image = output_root / "images" / f"{slug}.png"
    log_path.parent.mkdir(parents=True, exist_ok=True)
    started = time.monotonic()
    command = [
        str(ROOT / "run_minitune.sh"), row["expression"], slug,
        "--output-root", str(output_root),
        "--guidance-points", str(args.guidance_points),
        "--timeout", str(args.timeout),
        "--z3-jobs", "1",
        "--z3-engine", args.z3_engine,
        "--search-engine", args.search_engine,
        "--validator", args.validator,
        "--target-seconds", str(args.target_seconds),
        "--validation-jobs", str(args.validation_jobs),
        "--no-plot",
    ]
    if args.optuner_compatible:
        command.append("--optuner-compatible")
    if args.optuner_satire_compatible:
        command.append("--optuner-satire-compatible")
    command.append("--resume" if args.resume else "--no-resume")
    if row.get("mode") == "fixed":
        command[command.index("--guidance-points") + 1] = "0"
    try:
        result = subprocess.run(
            command, cwd=ROOT, text=True, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, timeout=args.timeout + 30, check=False,
        )
        log_text = result.stdout
        complete = (
            result.returncode == 0
            and frontier.exists()
            and (
                not (args.optuner_compatible or args.optuner_satire_compatible)
                or csv_has_data(frontier)
            )
        )
        status = "ok" if complete else "incomplete"
        if complete:
            reason = ""
        elif (
            (args.optuner_compatible or args.optuner_satire_compatible)
            and frontier.exists() and not csv_has_data(frontier)
        ):
            reason = "no certified candidate solutions"
        else:
            reason = f"exact pipeline exited with code {result.returncode}"
    except subprocess.TimeoutExpired as error:
        log_text = (error.stdout or "") if isinstance(error.stdout, str) else ""
        complete, status, reason = False, "timeout", f"exceeded {args.timeout} seconds"
    log_path.write_text(log_text)
    status_plotting = 0.0
    if not complete and not image.exists():
        status_plotting = status_image(image, row["name"], status, reason)

    checkpoint = output_root / "checkpoints" / slug / "summary.json"
    solver = json.loads(checkpoint.read_text()) if checkpoint.exists() else {}
    total = time.monotonic() - started
    preprocessing = float(solver.get("preprocessing_seconds") or 0.0)
    certification = float(solver.get("certification_seconds") or 0.0)
    validation = float(solver.get("validation_seconds") or 0.0)
    core_seconds = float(solver.get("seconds") or 0.0)
    satire = float(solver.get("satire_seconds") or 0.0)
    fallback = float(solver.get("fptaylor_fallback_seconds") or 0.0)
    plotting = extract_seconds(r"Plotting: ([0-9.]+)s", log_text)
    return {
        **row,
        "status": status,
        "reason": reason,
        "complete": complete,
        "preprocessing_seconds": preprocessing,
        "certification_seconds": certification,
        "z3_seconds": float(solver.get("z3_cpu_seconds") or 0.0),
        "dp_seconds": float(solver.get("dp_seconds") or 0.0),
        "z3_audit_seconds": float(solver.get("z3_audit_seconds") or 0.0),
        "satire_seconds": satire,
        "satire_dag_seconds": float(solver.get("satire_dag_seconds") or 0.0),
        "optimizer_calls": int(solver.get("optimizer_calls") or 0),
        "optimizer_seconds": float(solver.get("optimizer_seconds") or 0.0),
        "candidate_substitution_seconds": float(
            solver.get("candidate_substitution_seconds") or 0.0
        ),
        "fptaylor_fallback_seconds": (
            validation
            if solver.get("effective_validator") == "fptaylor"
            else fallback
        ),
        "satire_certified": int(solver.get("satire_certified") or 0),
        "satire_ambiguous": int(solver.get("satire_ambiguous") or 0),
        "fptaylor_fallback_count": int(
            solver.get("fptaylor_fallback_count") or 0
        ),
        "site_semantics": solver.get("site_semantics", ""),
        "validator_mode": solver.get("effective_validator", ""),
        "validation_seconds": validation,
        "plotting_seconds": plotting or status_plotting,
        "core_seconds": core_seconds,
        "total_seconds": total,
        "target_met": complete and core_seconds <= args.target_seconds,
        "z3_checks": int(solver.get("z3_checks") or 0),
        "z3_models": int(solver.get("z3_models") or 0),
        "frontier": str(frontier) if complete else "",
        "image": str(image),
        "log": str(log_path),
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, default=ROOT / "benchmarks/fpcore.json")
    parser.add_argument("--overrides", type=Path, default=ROOT / "benchmarks/domain_overrides.json")
    parser.add_argument("--output-root", type=Path, default=ROOT / "output/fpcore")
    parser.add_argument("--timeout", type=float, default=120.0)
    parser.add_argument("--jobs", type=int, default=2)
    parser.add_argument("--guidance-points", type=int, default=4)
    parser.add_argument("--z3-engine", choices=("pareto", "staircase"), default="pareto")
    parser.add_argument("--search-engine", choices=("auto", "dp", "z3"), default="auto")
    parser.add_argument(
        "--validator", choices=("auto", "fptaylor", "satire-hybrid"), default="auto"
    )
    parser.add_argument("--target-seconds", type=float, default=10.0)
    parser.add_argument("--list", action="store_true")
    parser.add_argument("--optuner-compatible", action="store_true")
    parser.add_argument("--optuner-satire-compatible", action="store_true")
    parser.add_argument(
        "--validation-jobs", type=int, default=min(8, max(1, (os.cpu_count() or 1))),
        help="Concurrent FPTaylor certifications within each benchmark",
    )
    parser.add_argument(
        "--resume", action=argparse.BooleanOptionalAction, default=True,
        help="Reuse matching exact-band checkpoints (default: enabled)",
    )
    parser.add_argument(
        "--cold-cache", action="store_true",
        help="Clear MiniTuner analysis caches before this suite run",
    )
    parser.add_argument(
        "--reference-root", type=Path,
        help="Compare completed compatibility frontiers with stored OpTuner fixtures",
    )
    args = parser.parse_args()
    if args.optuner_compatible and args.optuner_satire_compatible:
        parser.error(
            "--optuner-compatible and --optuner-satire-compatible are mutually exclusive"
        )
    if (args.optuner_compatible or args.optuner_satire_compatible) and args.validator != "auto":
        parser.error("--validator is available only in native mode")
    args.output_root = args.output_root.resolve()
    if args.cold_cache:
        shutil.rmtree(ROOT / ".cache/minituner", ignore_errors=True)
    for directory in ("inputs", "frontiers", "images", "logs", "checkpoints"):
        (args.output_root / directory).mkdir(parents=True, exist_ok=True)

    import_start = time.monotonic()
    import_command = [
            sys.executable, str(ROOT / "src/scripts/import_fpbench.py"),
            "--source", str(args.source), "--overrides", str(args.overrides),
            "--output-dir", str(args.output_root / "inputs"),
        ]
    if args.optuner_compatible or args.optuner_satire_compatible:
        import_command.append("--optuner-compatible")
    result = subprocess.run(import_command, cwd=ROOT, check=False)
    if result.returncode != 0:
        return result.returncode
    import_total = time.monotonic() - import_start
    manifest = json.loads((args.output_root / "inputs/manifest.json").read_text())
    if args.list:
        for row in manifest:
            print(f"{row['status']:<8} {row['name']}" + (f" -- {row['reason']}" if row['reason'] else ""))
        return 0

    rows: list[dict[str, Any]] = []
    ready = [row for row in manifest if row["status"] == "ready"]
    satire_worker = None
    satire_socket = args.output_root / "checkpoints" / "satire-worker.sock"
    use_satire = args.optuner_satire_compatible or (
        not args.optuner_compatible and args.validator != "fptaylor"
    )
    if use_satire:
        satire_socket.unlink(missing_ok=True)
        satire_worker = subprocess.Popen(
            [
                sys.executable,
                str(ROOT / "src/scripts/satire_validate.py"),
                "--server",
                str(satire_socket),
            ],
            cwd=ROOT,
        )
        deadline = time.monotonic() + 10
        while not satire_socket.exists() and time.monotonic() < deadline:
            time.sleep(0.01)
        if not satire_socket.exists():
            satire_worker.terminate()
            raise RuntimeError("SATIRE worker did not create its socket")
        os.environ["MINITUNER_SATIRE_SOCKET"] = str(satire_socket)
    try:
        with concurrent.futures.ThreadPoolExecutor(max_workers=max(1, args.jobs)) as executor:
            futures = {executor.submit(run_one, row, args): row for row in ready}
            for future in concurrent.futures.as_completed(futures):
                row = future.result()
                rows.append(row)
                print(
                    f"{row['status'].upper():<10} {row['name']} "
                    f"(core {row['core_seconds']:.2f}s)"
                )
    finally:
        os.environ.pop("MINITUNER_SATIRE_SOCKET", None)
        if satire_worker is not None:
            try:
                with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as client:
                    client.connect(str(satire_socket))
                    client.sendall(b'{"shutdown":true}\n')
                    client.recv(1024)
            except OSError:
                satire_worker.terminate()
            satire_worker.wait(timeout=5)

    # Plot completed frontiers after all timed tuner processes finish.  The
    # plotting module and Matplotlib stay loaded in this suite process.
    for row in rows:
        if not row["complete"]:
            continue
        plot_seconds = plot_frontier(Path(row["frontier"]), Path(row["image"]))
        row["plotting_seconds"] = plot_seconds
        row["total_seconds"] += plot_seconds

    for row in manifest:
        if row["status"] == "ready":
            continue
        image = args.output_root / "images" / f"benchmark_{row['index']:03d}.png"
        status = unsupported_status(row["reason"])
        plotting = status_image(image, row["name"], status, row["reason"])
        rows.append({
            **row, "status": status, "complete": False,
            "preprocessing_seconds": 0.0, "certification_seconds": 0.0,
            "z3_seconds": 0.0, "validation_seconds": 0.0,
            "dp_seconds": 0.0, "z3_audit_seconds": 0.0,
            "satire_seconds": 0.0, "fptaylor_fallback_seconds": 0.0,
            "satire_dag_seconds": 0.0, "optimizer_calls": 0,
            "optimizer_seconds": 0.0, "candidate_substitution_seconds": 0.0,
            "satire_certified": 0, "satire_ambiguous": 0,
            "fptaylor_fallback_count": 0,
            "site_semantics": (
                "optuner_deduplicated"
                if args.optuner_compatible or args.optuner_satire_compatible
                else "native_independent"
            ),
            "validator_mode": (
                "fptaylor" if args.optuner_compatible
                else "satire_hybrid"
                if args.optuner_satire_compatible or args.validator == "auto"
                else args.validator.replace("-", "_")
            ),
            "plotting_seconds": plotting,
            "core_seconds": 0.0,
            "total_seconds": row.get("import_seconds", 0.0) + plotting,
            "z3_checks": 0, "z3_models": 0, "frontier": "",
            "target_met": False,
            "image": str(image), "log": "",
        })

    rows.sort(key=lambda row: int(row["index"]))
    summary_fields = [
        "index", "name", "slug", "status", "reason", "complete",
        "domain_provenance", "mode", "region_count", "frontier", "image", "log",
        "timing_row",
    ]
    timing_fields = [
        "index", "name", "status", "import_seconds", "domain_inference_seconds", "preprocessing_seconds",
        "certification_seconds", "z3_seconds", "validation_seconds",
        "dp_seconds", "z3_audit_seconds", "satire_seconds",
        "satire_dag_seconds", "optimizer_calls", "optimizer_seconds",
        "candidate_substitution_seconds",
        "fptaylor_fallback_seconds", "target_met",
        "satire_certified", "satire_ambiguous", "fptaylor_fallback_count",
        "site_semantics", "validator_mode",
        "core_seconds", "plotting_seconds", "total_seconds", "z3_checks",
        "z3_models", "region_count",
    ]
    for row_number, row in enumerate(rows, start=2):
        row["timing_row"] = f"{args.output_root / 'timings.csv'}:{row_number}"
    for path, fields in (
        (args.output_root / "summary.csv", summary_fields),
        (args.output_root / "timings.csv", timing_fields),
    ):
        with path.open("w", newline="") as handle:
            writer = csv.DictWriter(handle, fieldnames=fields)
            writer.writeheader()
            writer.writerows({field: row.get(field, "") for field in fields} for row in rows)
    run = {
        "benchmarks": len(rows), "complete": sum(bool(row["complete"]) for row in rows),
        "unsupported_or_incomplete": sum(not bool(row["complete"]) for row in rows),
        "import_seconds": import_total, "total_seconds": sum(float(row["total_seconds"]) for row in rows),
        "output_root": str(args.output_root),
        "execution_mode": (
            "optuner_fptaylor" if args.optuner_compatible
            else "optuner_satire" if args.optuner_satire_compatible
            else "native"
        ),
        "site_semantics": (
            "optuner_deduplicated"
            if args.optuner_compatible or args.optuner_satire_compatible
            else "native_independent"
        ),
        "result_generation": "independent-minituner",
        "reference_role": "post-run-check-only" if args.reference_root else "none",
        "cache_mode": "cold" if args.cold_cache else "warm-or-existing",
        "resume": args.resume,
        "search_engine": args.search_engine,
        "validator": args.validator,
        "effective_validator": (
            "fptaylor" if args.optuner_compatible
            else "satire_hybrid"
            if args.optuner_satire_compatible or args.validator == "auto"
            else args.validator.replace("-", "_")
        ),
        "target_seconds": args.target_seconds,
        "target_met": sum(bool(row.get("target_met")) for row in rows),
    }
    (args.output_root / "run.json").write_text(json.dumps(run, indent=2) + "\n")
    print(json.dumps(run, indent=2))
    if args.reference_root:
        if not args.optuner_compatible:
            parser.error("--reference-root requires --optuner-compatible")
        check = subprocess.run(
            [
                sys.executable,
                str(ROOT / "src/scripts/check_optuner_reference.py"),
                "--actual-root", str(args.output_root),
                "--reference-root", str(args.reference_root),
            ],
            cwd=ROOT,
            check=False,
        )
        return check.returncode
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
