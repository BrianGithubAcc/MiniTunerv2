import sys
import time
from pathlib import Path
import pandas as pd
import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

def plot_frontier(filename: Path, output_file: Path) -> float:
    started = time.perf_counter()
    df = pd.read_csv(filename)
    df = df.dropna(subset=["error", "cost"])
    df = df.sort_values(["error", "cost"])

    x = df["error"]
    y = df["cost"]

    diagnostics_path = filename.with_name(f"{filename.stem}_diagnostics.csv")
    diagnostics = None
    if diagnostics_path.exists():
        diagnostics = pd.read_csv(diagnostics_path)

    plt.figure()
    if diagnostics is not None:
        statuses = (
            diagnostics["validation_status"]
            if "validation_status" in diagnostics.columns
            else pd.Series("certified", index=diagnostics.index)
        )
        certified_statuses = [
            "certified",
            "satire_certified",
            "fptaylor_certified",
            "dominated",
        ]
        validated = diagnostics[statuses.isin(certified_statuses)]
        failed = diagnostics[~statuses.isin(certified_statuses)]
        if "error" not in validated.columns and "validated_error" in validated.columns:
            validated = validated.rename(columns={"validated_error": "error"})
            failed = failed.rename(columns={"validated_error": "error"})
        validated_points = validated.dropna(subset=["error", "cost"])
        failed_points = failed.dropna(subset=["error", "cost"])
        if not validated_points.empty:
            plt.scatter(
                validated_points["error"], validated_points["cost"], s=14, color="0.65",
                label="certified band points", zorder=1,
            )
        if not failed_points.empty:
            plt.scatter(
                failed_points["error"], failed_points["cost"], marker="x", color="red",
                label="failed validation", zorder=3,
            )
    plt.plot(x, y, "o-", color="blue", label="exact Pareto frontier")
    plt.xscale("log")
    plt.xlabel("Total Error")
    plt.ylabel("Total Cost")
    #Assume / in path get last path (file name)
    if diagnostics is None:
        plt.title(filename.name)
    else:
        failed_count = int((~statuses.isin(certified_statuses)).sum())
        plt.title(
            f"{filename.name} — {len(diagnostics)} validated, "
            f"{len(df)} nondominated, {failed_count} failed"
        )
    plt.grid(True)
    plt.legend()

    output_file.parent.mkdir(parents=True, exist_ok=True)
    plt.savefig(output_file, dpi=300)
    plt.close()
    print(f"Plot saved to {output_file}")
    elapsed = time.perf_counter() - started
    print(f"Plotting: {elapsed:.3f}s")
    return elapsed


def main():
    if len(sys.argv) != 3:
        print("Usage: python plot.py <csv_file> <output_png>")
        sys.exit(1)
    plot_frontier(Path(sys.argv[1]), Path(sys.argv[2]).resolve())

if __name__ == "__main__":
    main()
