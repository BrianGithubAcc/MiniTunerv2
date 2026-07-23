#!/usr/bin/env python3
"""Regenerate OpTuner/MiniTuner frontier plots with overlap-visible markers."""

from __future__ import annotations

import argparse
import csv
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd


def read_rows(path: Path) -> list[dict[str, str]]:
    with path.open(newline="") as source:
        return list(csv.DictReader(source))


def close_pair(left: tuple[float, float], right: tuple[float, float]) -> bool:
    return bool(
        np.isclose(left[0], right[0], rtol=1e-8, atol=1e-300)
        and np.isclose(left[1], right[1], rtol=1e-10, atol=1e-12)
    )


def overlap_count(op_data: pd.DataFrame, mini_data: pd.DataFrame) -> int:
    unmatched = list(zip(mini_data["error"], mini_data["cost"]))
    count = 0
    for op_point in zip(op_data["error"], op_data["cost"]):
        for index, mini_point in enumerate(unmatched):
            if close_pair(op_point, mini_point):
                count += 1
                unmatched.pop(index)
                break
    return count


def load_frontier(row: dict[str, str]) -> pd.DataFrame:
    path = row.get("frontier", "")
    if row.get("complete") != "True" or not path or not Path(path).exists():
        return pd.DataFrame(columns=["error", "cost"])
    return pd.read_csv(path).dropna(subset=["error", "cost"])


def plot_case(
    destination: Path,
    name: str,
    op_row: dict[str, str],
    mini_row: dict[str, str],
    compatible: bool,
    reason: str,
) -> None:
    op_data = load_frontier(op_row) if compatible else pd.DataFrame()
    mini_data = load_frontier(mini_row) if compatible else pd.DataFrame()
    figure, axis = plt.subplots(figsize=(8.5, 5.4))

    if not op_data.empty or not mini_data.empty:
        if not op_data.empty:
            axis.plot(
                op_data["error"],
                op_data["cost"],
                linestyle="--",
                linewidth=1.8,
                color="#1f77b4",
                marker="o",
                markersize=9,
                markerfacecolor="none",
                markeredgecolor="#1f77b4",
                markeredgewidth=2.0,
                label="OpTuner",
                zorder=2,
            )
        if not mini_data.empty:
            axis.plot(
                mini_data["error"],
                mini_data["cost"],
                linestyle="-",
                linewidth=1.25,
                color="#ff7f0e",
                marker="x",
                markersize=7,
                markeredgewidth=2.0,
                label="MiniTuner",
                zorder=3,
            )

        all_errors = pd.concat(
            [data["error"] for data in (op_data, mini_data) if not data.empty]
        )
        if (all_errors > 0).all():
            axis.set_xscale("log")
        else:
            axis.set_xscale("symlog", linthresh=1e-300)
        axis.set_xlabel("FPTaylor-validated absolute error")
        axis.set_ylabel("Modeled implementation cost")
        axis.grid(True, alpha=0.25)
        axis.legend()

        subtitle = f"OpTuner: {op_row['status']} | MiniTuner: {mini_row['status']}"
        if not op_data.empty and not mini_data.empty:
            overlap = overlap_count(op_data, mini_data)
            subtitle += (
                f"\nMatched points: {overlap} "
                f"(OpTuner {len(op_data)}, MiniTuner {len(mini_data)})"
            )
        axis.set_title(f"{name}\n{subtitle}")
    else:
        axis.axis("off")
        heading = (
            "INCOMPARABLE IMPLEMENTATION DATA"
            if not compatible
            else "NO SHARED COMPLETE FRONTIER"
        )
        detail = reason if not compatible else (
            f"OpTuner: {op_row.get('status', 'missing')}\n"
            f"MiniTuner: {mini_row.get('status', 'missing')}"
        )
        axis.text(0.5, 0.68, name, ha="center", fontsize=16, weight="bold")
        axis.text(0.5, 0.49, heading, ha="center", fontsize=13, color="darkred")
        axis.text(0.5, 0.30, detail, ha="center", fontsize=10, wrap=True)

    figure.tight_layout()
    destination.parent.mkdir(parents=True, exist_ok=True)
    figure.savefig(destination, dpi=180)
    plt.close(figure)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--output-root",
        type=Path,
        default=Path("output/optuner_comparison"),
    )
    args = parser.parse_args()
    root = args.output_root.resolve()

    comparison = read_rows(root / "summary.csv")
    op_rows = {
        int(row["index"]): row
        for row in read_rows(root / "runs/optuner/summary.csv")
    }
    mini_rows = {
        int(row["index"]): row
        for row in read_rows(root / "runs/minituner/summary.csv")
    }

    for row in comparison:
        index = int(row["index"])
        compatible = row["status"] != "incomparable-implementation-data"
        plot_case(
            root / "plots" / f"{row['slug']}.png",
            row["name"],
            op_rows[index],
            mini_rows[index],
            compatible,
            row.get("compatibility_reason", ""),
        )

    print(f"Regenerated {len(comparison)} overlap-visible plots under {root / 'plots'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
