import os
import re
import matplotlib.pyplot as plt
import numpy as np

#I am to lazy to import the txt file
#with remove_ordering
minitune = r"""Time of exp1 (average over 3 runs): 5.53667 s
Time of exp1m (average over 3 runs): 4.88667 s
Time of log1p (average over 3 runs): 2.37 s
Time of exp1x_log (average over 3 runs): 214.143 s
Time of logexp (average over 3 runs): 6.29333 s
Time of nmse_example_3_4 (average over 3 runs): 26.11 s
Time of nmse_example_3_7 (average over 3 runs): 4.70667 s
Time of nmse_example_3_8 (average over 3 runs): 16.4767 s
Time of nmse_example_3_10 (average over 3 runs): 3.94 s
Time of nmse_problem_3_3_6 (average over 3 runs): 12.1167 s
Time of nmse_problem_3_3_7 (average over 3 runs): 316.52 s
Time of nmse_section_3_11 (average over 3 runs): 4.59333 s"""
#without
optuner = r"""Time of exp1 (average over 3 runs): 5.89 s
Time of exp1m (average over 3 runs): 5.56 s
Time of log1p (average over 3 runs): 2.25 s
Time of exp1x_log (average over 3 runs): 184.06 s
Time of logexp (average over 3 runs): 10.48 s
Time of nmse_example_3_4 (average over 3 runs): 33.94 s
Time of nmse_example_3_7 (average over 3 runs): 6.13 s
Time of nmse_example_3_8 (average over 3 runs): 15.8 s
Time of nmse_example_3_10 (average over 3 runs): 34.4367 s
Time of nmse_problem_3_3_6 (average over 3 runs): 15.76 s
Time of nmse_problem_3_3_7 (average over 3 runs): 390.007 s
Time of nmse_section_3_11 (average over 3 runs): 5.80333 s"""

minitune_data = {}
for line in minitune.split('\n'):
    parts = line.split(' ')
    name = parts[2]
    value = float(parts[-2])
    minitune_data[name] = value

optuner_data={}
for line in optuner.split('\n'):
    parts = line.split(' ')
    name = parts[2]
    value = float(parts[-2])
    optuner_data[name] = value

labels = sorted(set(minitune_data.keys()) | set(optuner_data.keys()))
x = np.arange(len(labels))
width = 0.35

mini_vals = [minitune_data.get(l, 0) for l in labels]
opt_vals = [optuner_data.get(l, 0) for l in labels]

fig, ax = plt.subplots(figsize=(12, 6))
ax.bar(x - width/2, mini_vals, width, label='remove_odering', color='#1f77b4', zorder=3)
ax.bar(x + width/2, opt_vals, width, label='no remove_ordering', color='#ff7f0e', zorder=3)

ax.set_xlabel("Benchmark / File", fontweight='bold')
ax.set_ylabel("Runtime (s)", fontweight='bold')
ax.set_xticks(x)
ax.set_xticklabels(labels, rotation=45, ha='right')
ax.grid(axis='y', linestyle='--', alpha=0.7, zorder=0)
for spine in ax.spines.values():
    spine.set_visible(False)

ax.set_title("ordered vs unordered - Runtime", fontsize=14, fontweight='bold', pad=15)
ax.legend(loc='upper left', bbox_to_anchor=(-0.05, -0.175), ncol=2, frameon=False, fontsize=12)

plt.tight_layout()

output_dir = os.path.join(os.path.dirname(__file__), "..", "..", "images")
os.makedirs(output_dir, exist_ok=True)
output_path = os.path.join(output_dir, "ordering_runtime.pdf")
plt.savefig(output_path)
plt.close()
print(f"Saved: {output_path}")