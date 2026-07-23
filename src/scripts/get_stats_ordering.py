import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import math

def process_file(filepath):
    df = pd.read_csv(filepath)
    df = df.dropna(subset=["error", "cost"])
    num_points = len(df)
    min_distance = np.sqrt(df["error"] + (df["cost"]*1e-8) **2).min()
    return num_points, min_distance

def main():
    dir_path = os.path.dirname(os.path.realpath(__file__))

    results_points = {"miniTune_without_remove_ordering": {}, "miniTune_with_remove_ordering": {}}
    results_min = {"miniTune_without_remove_ordering": {}, "miniTune_with_remove_ordering": {}}

    for method in ("miniTune_without_remove_ordering", "miniTune_with_remove_ordering"):
        #USe relative as I moved to working in a codespace
        target_dir = os.path.join(dir_path, "..", "..", "output", method)
        for filename in os.listdir(target_dir):
            if not filename.endswith(".csv"):
                continue
            filepath = os.path.join(target_dir, filename)
            num_points, min_distance = process_file(filepath)
            name = filename.split(".")[0]
            results_points[method][name] = num_points
            results_min[method][name] = min_distance

    labels = sorted(set(results_points["miniTune_without_remove_ordering"]) | set(results_points["miniTune_with_remove_ordering"]))
    x = np.arange(len(labels))
    width = 0.35

    mini_vals = [results_points["miniTune_without_remove_ordering"].get(l, 0) for l in labels]
    opt_vals  = [results_points["miniTune_with_remove_ordering"].get(l, 0) for l in labels]

    mini_min = [results_min["miniTune_without_remove_ordering"].get(l, float("nan")) for l in labels]
    opt_min  = [results_min["miniTune_with_remove_ordering"].get(l, float("nan")) for l in labels]

    output_dir = os.path.join(dir_path, "..", "..", "images")
    os.makedirs(output_dir, exist_ok=True)

    colors = ["#1f77b4", "#ff7f0e"]

    #No point
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.bar(x - width/2, mini_vals, width, label="unordered", color=colors[0], zorder=3)
    ax.bar(x + width/2, opt_vals, width, label="ordered", color=colors[1], zorder=3)

    ax.set_xlabel("Benchmark / File", fontweight="bold")
    ax.set_ylabel("Number of Points", fontweight="bold")
    ax.set_xticks(x)
    ax.set_xticklabels(labels, rotation=45, ha='right')
    ax.grid(axis='y', linestyle='--', alpha=0.7, zorder=0)
    for spine in ax.spines.values():
        spine.set_visible(False)

    ax.set_title("unordered vs ordered - Number of Points", fontsize=14, fontweight='bold', pad=15)

    ax.legend(
        loc='upper left',
        bbox_to_anchor=(-0.05, -0.175),
        ncol=2,
        frameon=False,
        fontsize=12
    )

    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "comparison_points_ordering.pdf"))
    plt.close()
    print("Saved: comparison_points.pdf")

    #Min distance
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.bar(x - width/2, mini_min, width, label="unordered", color=colors[0], zorder=3)
    ax.bar(x + width/2, opt_min, width, label="ordered", color=colors[1], zorder=3)

    ax.set_xlabel("Benchmark / File", fontweight="bold")
    ax.set_ylabel(r"Min $\sqrt{\mathrm{error}^2 + \mathrm{cost}^2}$", fontweight="bold")
    ax.set_xticks(x)
    ax.set_yscale("log")
    ax.set_xticklabels(labels, rotation=45, ha='right')
    ax.grid(axis='y', linestyle='--', alpha=0.7, zorder=0)
    for spine in ax.spines.values():
        spine.set_visible(False)

    ax.set_title("unordered vs ordered - Minimum Distance", fontsize=14, fontweight='bold', pad=15)

    ax.legend(
        loc='upper left',
        bbox_to_anchor=(-0.05, -0.175),
        ncol=2,
        frameon=False,
        fontsize=12
    )

    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, "comparison_min_distance_ordering.pdf"))
    plt.close()
    print("Saved: comparison_min_distance.pdf")

    print(results_min.items())
        
if __name__ == "__main__":
    main()