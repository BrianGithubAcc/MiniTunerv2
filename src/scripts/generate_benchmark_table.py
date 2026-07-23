#!/usr/bin/env python3
"""Generate the checked-in OpTuner/MiniTuner benchmark summary."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path


def read_rows(path: Path) -> dict[int, dict[str, str]]:
    if not path.exists():
        return {}
    with path.open(newline="") as stream:
        return {int(row["index"]): row for row in csv.DictReader(stream)}


def elapsed(row: dict[str, str] | None) -> float | None:
    if not row:
        return None
    for key in ("tuner_seconds", "core_seconds"):
        try:
            return float(row[key])
        except (KeyError, TypeError, ValueError):
            pass
    try:
        return max(
            0.0,
            float(row["total_seconds"]) - float(row.get("plotting_seconds") or 0),
        )
    except (KeyError, TypeError, ValueError):
        return None


def status_cell(
    summary: dict[str, str] | None, timing: dict[str, str] | None
) -> str:
    status = (
        (timing or {}).get("status")
        or (summary or {}).get("status")
        or "not-run"
    ).strip()
    complete = (summary or {}).get("complete", "").lower() == "true"
    succeeded = status == "ok" or complete
    seconds = elapsed(timing)
    if succeeded and seconds is not None:
        return f"✅ {seconds:.3f} s"
    if succeeded:
        return "✅ implemented"
    reason = ((summary or {}).get("reason") or status).strip().replace("|", "/")
    return f"❌ {reason}"


def status_and_time(
    summary: dict[str, str] | None, timing: dict[str, str] | None
) -> tuple[str, str]:
    combined = status_cell(summary, timing)
    seconds = elapsed(timing)
    if combined.startswith("✅"):
        return ("✅", f"{seconds:.3f} s" if seconds is not None else "—")
    return (combined, "—")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--optuner-root", type=Path, default=Path("output/optuner"))
    parser.add_argument(
        "--fptaylor-root",
        type=Path,
        default=Path("output/minituner_optuner_compatible_fptaylor"),
    )
    parser.add_argument(
        "--satire-root",
        type=Path,
        default=Path("output/minituner_optuner_compatible_satire"),
    )
    parser.add_argument("--output", type=Path, default=Path("BENCHMARK_RESULTS.md"))
    args = parser.parse_args()

    roots = [args.optuner_root, args.fptaylor_root, args.satire_root]
    summaries = [read_rows(root / "summary.csv") for root in roots]
    timings = [read_rows(root / "timings.csv") for root in roots]
    indexes = sorted(set().union(*(set(rows) for rows in summaries + timings)))

    lines = [
        "# OpTuner and MiniTuner benchmark results",
        "",
        "The time in each successful cell is core tuning time: importing, exact",
        "search, and rigorous validation, excluding plot generation where the",
        "runner records it separately. A check without a time means the backend",
        "is implemented but its timing row is unavailable. A cross reports the",
        "suite's explicit failure or unsupported status.",
        "",
        "| # | Benchmark | OpTuner status | Time | MiniTuner + FPTaylor status | Time | MiniTuner + SATIRE status | Time |",
        "|---:|---|---|---:|---|---:|---|---:|",
    ]
    for index in indexes:
        name = next(
            (
                rows[index]["name"]
                for rows in summaries + timings
                if index in rows and rows[index].get("name")
            ),
            f"benchmark-{index}",
        ).replace("|", "/")
        cells = [
            status_and_time(
                summaries[column].get(index), timings[column].get(index)
            )
            for column in range(3)
        ]
        lines.append(
            f"| {index} | {name} | {cells[0][0]} | {cells[0][1]} "
            f"| {cells[1][0]} | {cells[1][1]} "
            f"| {cells[2][0]} | {cells[2][1]} |"
        )

    complete_counts = [
        sum(
            1
            for index in indexes
            if status_cell(summary.get(index), timing.get(index)).startswith("✅")
        )
        for summary, timing in zip(summaries, timings)
    ]
    lines.extend(
        [
            "",
            "## Coverage",
            "",
            f"- OpTuner: {complete_counts[0]}/{len(indexes)}",
            f"- MiniTuner + FPTaylor: {complete_counts[1]}/{len(indexes)}",
            f"- MiniTuner + SATIRE: {complete_counts[2]}/{len(indexes)}",
            "",
            "## Accuracy note",
            "",
            "Historical OpTuner and MiniTuner+FPTaylor plots may contain the",
            "legacy exponent-zero conversion that disabled a nonzero absolute",
            "delta term. Regenerate the FPTaylor suite with the current code",
            "before comparing error bounds. Times above remain the recorded",
            "runtime measurements from their respective suite runs.",
            "",
            "Regenerate this file after running the three suites:",
            "",
            "```bash",
            "python src/scripts/generate_benchmark_table.py",
            "```",
            "",
        ]
    )
    args.output.write_text("\n".join(lines))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
