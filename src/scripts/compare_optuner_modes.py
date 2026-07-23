#!/usr/bin/env python3
"""Compare OpTuner with MiniTuner's FPTaylor and SATIRE compatibility modes."""

from __future__ import annotations

import argparse
import csv
import json
import math
from pathlib import Path
from typing import Any

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


def rows(path: Path) -> list[dict[str, str]]:
    if not path.exists():
        return []
    with path.open(newline="") as handle:
        return list(csv.DictReader(handle))


def indexed(path: Path) -> dict[int, dict[str, str]]:
    return {int(row["index"]): row for row in rows(path)}


def number(row: dict[str, str], *names: str) -> float | None:
    for name in names:
        value = row.get(name, "").strip()
        if value:
            try:
                result = float(value)
                if math.isfinite(result):
                    return result
            except ValueError:
                pass
    return None


def complete(row: dict[str, str] | None) -> bool:
    if not row:
        return False
    return row.get("status", "").lower() == "ok" or row.get(
        "complete", ""
    ).lower() == "true"


def frontier(root: Path, slug: str) -> tuple[np.ndarray, np.ndarray]:
    data = rows(root / "frontiers" / f"{slug}.csv")
    points = []
    for row in data:
        cost = number(row, "cost")
        error = number(row, "error", "validated_error")
        if cost is not None and error is not None and error > 0:
            points.append((cost, error))
    points = sorted(set(points))
    if not points:
        return np.array([]), np.array([])
    cost, error = zip(*points)
    return np.asarray(cost), np.asarray(error)


def timing(root: Path, index: int, kind: str) -> float | None:
    table = indexed(root / "timings.csv")
    row = table.get(index)
    if not row or row.get("status", "").lower() != "ok":
        return None
    if kind == "optuner":
        return number(row, "tuner_seconds", "total_seconds")
    if kind == "satire":
        return number(row, "core_seconds", "total_seconds")
    # Older FPTaylor reports predate core_seconds. Exclude plotting where
    # possible so the three bars measure tuning rather than rendering.
    core = number(row, "core_seconds")
    if core is not None:
        return core
    total = number(row, "total_seconds")
    plotting = number(row, "plotting_seconds") or 0.0
    return None if total is None else max(0.0, total - plotting)


def plot_benchmark(
    path: Path,
    name: str,
    index: int,
    slug: str,
    roots: dict[str, Path],
    times: dict[str, float | None],
) -> str:
    styles = {
        "OpTuner": dict(color="#1f77b4", marker="o", linestyle="-", zorder=1),
        "MiniTuner + FPTaylor": dict(
            color="#ff7f0e", marker="s", linestyle="--", zorder=2
        ),
        "MiniTuner + SATIRE": dict(
            color="#2ca02c", marker="x", linestyle=":", zorder=3
        ),
    }
    figure, (frontier_axis, runtime_axis) = plt.subplots(
        1, 2, figsize=(12, 4.8), gridspec_kw={"width_ratios": [2.2, 1]}
    )
    plotted = 0
    for label, root in roots.items():
        costs, errors = frontier(root, slug)
        if not len(costs):
            continue
        style = styles[label]
        frontier_axis.plot(
            errors,
            costs,
            label=f"{label} ({len(costs)} points)",
            linewidth=1.7,
            markersize=5.5,
            markerfacecolor=(
                "none" if style["marker"] in {"o", "s"} else style["color"]
            ),
            markeredgewidth=1.5,
            **style,
        )
        plotted += 1
    if plotted:
        frontier_axis.set_xscale("log")
        frontier_axis.set_xlabel("Validated absolute error")
        frontier_axis.set_ylabel("Modelled implementation cost")
        frontier_axis.grid(True, which="both", alpha=0.25)
        frontier_axis.legend(fontsize=8)
    else:
        frontier_axis.axis("off")
        frontier_axis.text(
            0.5, 0.5, "No completed frontier", ha="center", va="center"
        )

    labels = list(roots)
    values = [times[label] for label in labels]
    positions = np.arange(len(labels))
    heights = [value if value is not None and value > 0 else np.nan for value in values]
    runtime_axis.bar(
        positions,
        heights,
        color=[styles[label]["color"] for label in labels],
        width=0.68,
    )
    runtime_axis.set_yscale("log")
    runtime_axis.set_ylabel("Core tuning time (seconds, log scale)")
    runtime_axis.set_xticks(positions)
    runtime_axis.set_xticklabels(
        ["OpTuner", "MiniTuner\nFPTaylor", "MiniTuner\nSATIRE"], fontsize=8
    )
    runtime_axis.grid(True, axis="y", which="both", alpha=0.25)
    for position, value in zip(positions, values):
        if value is None:
            runtime_axis.text(position, 1.0, "N/A", ha="center", va="bottom")
        else:
            runtime_axis.text(
                position,
                value,
                f"{value:.3g}s",
                ha="center",
                va="bottom",
                fontsize=8,
            )
    figure.suptitle(f"{index:03d}: {name}", fontsize=11)
    figure.tight_layout()
    path.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(path, dpi=200)
    plt.close(figure)
    return "plotted" if plotted else "status-only"


def global_runtime_plot(
    path: Path, names: list[str], values: dict[str, list[float | None]]
) -> None:
    valid_indexes = [
        index
        for index in range(len(names))
        if all(values[label][index] is not None for label in values)
    ]
    labels = [names[index] for index in valid_indexes]
    figure, axis = plt.subplots(
        figsize=(max(12, 0.38 * len(valid_indexes)), 6)
    )
    width = 0.26
    x = np.arange(len(valid_indexes))
    colors = ["#1f77b4", "#ff7f0e", "#2ca02c"]
    for offset, (label, data), color in zip((-width, 0, width), values.items(), colors):
        axis.bar(
            x + offset,
            [data[index] for index in valid_indexes],
            width,
            label=label,
            color=color,
        )
    axis.set_yscale("log")
    axis.set_ylabel("Core tuning time (seconds, log scale)")
    axis.set_xticks(x)
    axis.set_xticklabels(labels, rotation=70, ha="right", fontsize=7)
    axis.grid(True, axis="y", which="both", alpha=0.25)
    axis.legend()
    figure.tight_layout()
    path.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(path, dpi=200)
    plt.close(figure)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--optuner-root", type=Path, required=True)
    parser.add_argument("--fptaylor-root", type=Path, required=True)
    parser.add_argument("--satire-root", type=Path, required=True)
    parser.add_argument("--output-root", type=Path, required=True)
    args = parser.parse_args()
    roots = {
        "OpTuner": args.optuner_root.resolve(),
        "MiniTuner + FPTaylor": args.fptaylor_root.resolve(),
        "MiniTuner + SATIRE": args.satire_root.resolve(),
    }
    summaries = {label: indexed(root / "summary.csv") for label, root in roots.items()}
    all_indexes = sorted(set().union(*(table.keys() for table in summaries.values())))
    output = args.output_root.resolve()
    plots = output / "plots"
    comparison_rows: list[dict[str, Any]] = []
    runtime_values = {label: [] for label in roots}
    runtime_names: list[str] = []
    for index in all_indexes:
        records = {label: table.get(index) for label, table in summaries.items()}
        name = next(
            (record["name"] for record in records.values() if record), f"benchmark {index}"
        )
        slug = next(
            (record["slug"] for record in records.values() if record), f"benchmark_{index:03d}"
        )
        times = {
            "OpTuner": timing(roots["OpTuner"], index, "optuner"),
            "MiniTuner + FPTaylor": timing(
                roots["MiniTuner + FPTaylor"], index, "fptaylor"
            ),
            "MiniTuner + SATIRE": timing(
                roots["MiniTuner + SATIRE"], index, "satire"
            ),
        }
        plot_path = plots / f"{slug}.png"
        plot_kind = plot_benchmark(plot_path, name, index, slug, roots, times)
        runtime_names.append(slug)
        for label in roots:
            runtime_values[label].append(times[label])
        comparison_rows.append(
            {
                "index": index,
                "name": name,
                "slug": slug,
                "optuner_status": records["OpTuner"].get("status", "missing")
                if records["OpTuner"]
                else "missing",
                "minituner_fptaylor_status": records[
                    "MiniTuner + FPTaylor"
                ].get("status", "missing")
                if records["MiniTuner + FPTaylor"]
                else "missing",
                "minituner_satire_status": records["MiniTuner + SATIRE"].get(
                    "status", "missing"
                )
                if records["MiniTuner + SATIRE"]
                else "missing",
                "optuner_seconds": times["OpTuner"],
                "minituner_fptaylor_seconds": times["MiniTuner + FPTaylor"],
                "minituner_satire_seconds": times["MiniTuner + SATIRE"],
                "plot_kind": plot_kind,
                "plot": str(plot_path),
            }
        )
    output.mkdir(parents=True, exist_ok=True)
    fields = list(comparison_rows[0]) if comparison_rows else []
    with (output / "summary.csv").open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(comparison_rows)
    global_runtime_plot(output / "runtime_bar.png", runtime_names, runtime_values)
    run = {
        "benchmarks": len(comparison_rows),
        "optuner_complete": sum(
            complete(summaries["OpTuner"].get(index)) for index in all_indexes
        ),
        "minituner_fptaylor_complete": sum(
            complete(summaries["MiniTuner + FPTaylor"].get(index))
            for index in all_indexes
        ),
        "minituner_satire_complete": sum(
            complete(summaries["MiniTuner + SATIRE"].get(index))
            for index in all_indexes
        ),
        "roots": {label: str(root) for label, root in roots.items()},
    }
    (output / "run.json").write_text(json.dumps(run, indent=2) + "\n")
    print(json.dumps(run, indent=2))
    print(f"Comparison plots: {plots}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
