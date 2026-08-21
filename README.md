
Testing capabilities of antigenic coding.

Pros:

- Followed instructions
- Fast
- can try alot of different ideas quickly

Cons:

- Needs hand held
- Code luggage is very easy to accumulate
- Code needs review

End result is this repo.
------------------------------------------

# MiniTuner

MiniTuner selects elementary-function implementations on an exact cost/error
Pareto frontier. Exact rational dynamic programming constructs the modeled
frontier, Z3 audits assignments, and a SATIRE-assisted directed-MPFR remainder
analysis certifies candidate errors. SATIRE modes never invoke FPTaylor:
unsupported or unresolved candidates are reported as infeasible or incomplete.
FPTaylor remains available through its explicit validator and compatibility
mode. No statistical guidance phase is used.

The latest per-benchmark coverage and core timings are in
[BENCHMARK_RESULTS.md](BENCHMARK_RESULTS.md).

## How the fast pipeline works

MiniTuner separates discrete implementation search from rigorous numerical
validation:

```text
parse expression and domains once
  -> build stable function sites and eligible candidate rows
  -> exact-rational Pareto dynamic programming
  -> one batched Z3 assignment audit
  -> one shared SATIRE expression-DAG analysis
  -> exact epsilon/delta substitution for all candidate assignments
  -> directed-MPFR remainder and adaptive region refinement
  -> exact final dominance reduction
```

### Exact rational dynamic programming

If site \(i\) has candidate implementations \(I_i\), the DP starts with the
zero-cost, zero-error state and adds one site at a time. Each state records its
exact rational cost, modeled error, and assignment. After a site is added,
states dominated in both cost and modeled error are removed. Equal objective
pairs are consolidated deterministically.

This enumerates the complete modeled Pareto frontier without asking Z3 to
repeatedly optimize the same Boolean selection problem. In OpTuner-compatible
modes, structurally identical calls share one implementation choice. Native
MiniTuner assigns a separate choice to every call occurrence. Z3 is retained
as a correctness auditor: MiniTuner submits the complete DP assignment set in
one batch and checks the exactly-one and eligibility constraints.

### Why SATIRE validation is fast

The FPTaylor path constructs external analysis queries and invokes
FPTaylor/Gelpia during preprocessing and candidate validation. Process startup,
symbolic Taylor construction, and global optimization dominate its runtime,
especially when a frontier has many candidate assignments.

The SATIRE path does not invoke FPTaylor or Gelpia. It:

- parses and canonicalizes the expression DAG once;
- propagates signed, site-specific first-order noise symbols;
- accepts arbitrary candidate relative/absolute bounds
  \(\epsilon_i |f_i|+\delta_i\);
- retains nonlinear, second-order, and cross-site terms in a separately
  directed-rounded remainder;
- substitutes all candidate epsilon/delta tuples into the shared analysis;
- validates candidates concurrently;
- spends the deeper subdivision budget on the low-error accuracy anchors and
  the region currently determining the global bound; and
- chooses subdivision variables by their measured effect on the certified
  output bound, rather than input width alone.

The scalar interval enclosure remains an emergency conservative bound.
SATIRE never reports a candidate when its domain, denominator, overflow, or
remainder cannot be enclosed. This is symbolic rigorous analysis, not machine
learning.

### Comparing SATIRE and FPTaylor errors

Both compatibility modes now use the same conservative local epsilon/delta
encoding. FPTaylor reserves exponent zero to mean that an error component is
absent; MiniTuner explicitly shifts nonzero components away from that sentinel
instead of accidentally disabling them. SATIRE reconstructs the same encoded
local model before validation.

The final numerical bounds need not be bit-identical: SATIRE and FPTaylor use
different conservative global bounding algorithms. SATIRE may therefore be
slightly higher or lower. A valid comparison requires matching assignments,
candidate specifications, domains, and a freshly generated FPTaylor reference.
Old FPTaylor results produced before the sentinel fix must not be used as an
accuracy oracle.

## Setup

```bash
nix develop path:.
minituner-setup
cd src && dune build
```

## Run one expression

From the repository root:

```bash
./run_minitune.sh \
  "x in [0.0078125,0.5];(exp(x) - 1)/x" \
  exp1 --guidance-points 4
```

Outputs are written to:

```text
output/fpcore/frontiers/exp1.csv
output/fpcore/images/exp1.png
output/fpcore/checkpoints/exp1/
```

Use `--no-guidance` to disable exact candidate cuts for a correctness or timing
baseline. Other useful options are `--timeout`, `--z3-engine`, `--z3-jobs`,
`--resume`, and `--no-resume`.

The default uses independent function occurrences. Thus two identical calls
may select different implementations. `--validator fptaylor` and
`--validator satire-hybrid` are native-mode testing overrides; `auto` selects
SATIRE hybrid.

## OpTuner-compatible mode

MiniTuner's native mode treats repeated function sites independently. To use
MiniTuner's independent reimplementation of OpTuner's duplicate-sharing,
stable site IDs, candidate rows, exact decimal costs, Pareto engine, structural
FPTaylor-form matching, and validation conventions, add:

```bash
./run_minitune.sh \
  "x in [0.0078125,0.5];(exp(x) - 1)/x" \
  exp1_optuner --optuner-compatible --timeout 300
```

For the complete imported OpTuner suite:

```bash
./run_all_fpcore.sh \
  --source benchmarks/optuner.fpcore \
  --output-root output/minituner_optuner_compatible_fptaylor \
  --optuner-compatible \
  --timeout 300 --jobs 1 --validation-jobs 8
```

Compatibility runs fail instead of reporting completion when no candidate
passes FPTaylor validation. Exact costs, validated errors, stable assignments,
and modeled-error diagnostics are written in OpTuner-compatible CSV columns.

To retain the same OpTuner-style deduplicated assignments and exact costs while
using the faster rigorous SATIRE-plus-remainder validator, use:

```bash
./run_minitune.sh \
  "x in [0.0078125,0.5];(exp(x) - 1)/x" \
  exp1_optuner_satire --optuner-satire-compatible --timeout 300
```

The two compatibility flags are mutually exclusive and lock their validator:
`--optuner-compatible` selects FPTaylor, while
`--optuner-satire-compatible` selects SATIRE hybrid.

Frozen results from an unchanged OpTuner checkout can be refreshed explicitly:

```bash
python src/scripts/refresh_optuner_references.py \
  --optuner-output output/optuner \
  --input-root output/minituner_optuner_compatible_fptaylor/inputs \
  --reference-root benchmarks/optuner_reference \
  --optuner-root ../OpTuner
```

Then run MiniTuner alone and compare its already-produced results with those
fixtures:

```bash
./run_all_fpcore.sh \
  --source benchmarks/optuner.fpcore \
  --output-root output/minituner_optuner_compatible_satire \
  --optuner-satire-compatible \
  --reference-root benchmarks/optuner_reference \
  --timeout 300 --jobs 1
```

Use `--cold-cache` for a cold-cache timing run. Without it, timings are labelled
as warm or existing-cache measurements in `run.json`.

`--validation-jobs` controls MiniTuner's concurrent candidate certification in
both backends. It does not use OpTuner results: each Z3 assignment is
independently rebuilt and certified by MiniTuner. With suite `--jobs 1`, values
from 8 to 12 are suitable for a 16-thread machine.

Reference fixtures are optional regression-test oracles only. They are read
after MiniTuner has completed and written its frontier, and never participate
in candidate selection, Z3 constraints, FPTaylor validation, dominance
reduction, or plotting.

## Run the FPCore suite

```bash
nix develop path:. --command ./run_all_fpcore.sh \
  --timeout 120 --jobs 2 --guidance-points 4
```

For OpTuner-compatible exact search, the fast independent configuration is:

```bash
nix develop path:. --command ./run_all_fpcore.sh \
  --source benchmarks/optuner.fpcore \
  --output-root output/minituner_optuner_compatible_fptaylor \
  --optuner-compatible \
  --search-engine auto \
  --validator auto \
  --target-seconds 10 \
  --timeout 300 \
  --jobs 1 \
  --validation-jobs 12 \
  --no-resume \
  --cold-cache
```

To regenerate the three-way plots from the retained OpTuner, MiniTuner
FPTaylor-compatible, and MiniTuner SATIRE-compatible results:

```bash
nix develop path:. --command python src/scripts/compare_optuner_modes.py \
  --optuner-root output/optuner \
  --fptaylor-root output/minituner_optuner_compatible_fptaylor \
  --satire-root output/minituner_optuner_compatible_satire \
  --output-root output/optuner_comparison
```

Each benchmark image overlays all available cost/error frontiers and includes
a core-runtime bar chart. The comparison directory also contains a global
log-scale runtime chart and a status/runtime CSV for all 41 benchmarks.

Generate the checked-in Markdown timing and implementation-status table with:

```bash
python src/scripts/generate_benchmark_table.py
```

Successful cells contain core tuning time. Failed or unsupported cells contain
an explicit cross and the suite's reason. Plotting time is excluded whenever
the runner reports it separately.

`--search-engine auto` selects exact rational dynamic programming in native
and compatibility modes, falling back to Z3 when a domain constraint cannot be
reduced exactly. `--search-engine z3` remains the reference baseline.
Every DP objective pair is checked in a lightweight Z3 assignment model.

SATIRE v1.1 is pinned by the flake and patched locally to accept arbitrary
per-node relative/absolute noise. Its directed-MPFR analysis keeps signed,
site-specific first-order coefficients and a separate nonlinear/cross-site
remainder, while a scalar interval enclosure remains an emergency conservative
bound. Successful regions are adaptively subdivided when that tightens the
reported bound. SATIRE-mode diagnostics record both bounds, the remainder,
region count, dominant site, and stopping reason. `--target-seconds` records the
performance target and never weakens or terminates certification.

Inspect import/domain status without solving:

```bash
./run_all_fpcore.sh --list
```

All generated suite artifacts are placed under `output/fpcore/`:

```text
inputs/       converted MiniTuner expressions and import manifest
frontiers/    complete rational Pareto CSVs and diagnostics
images/       frontier plots or explicit status images
logs/         per-benchmark logs
checkpoints/  resumable exact-solver state
summary.csv   status and artifact path for every FPCore entry
timings.csv   phase-separated timing and solver statistics
run.json      aggregate run metadata
```

Source domains are preserved. Missing bounds are filled only when MiniTuner
can infer a conservative finite binary64-valid box. Disconnected, unresolved,
non-finite, unsupported-precision, and unsupported-control-flow cases receive a
status image and never a falsely complete frontier.

## Tests

```bash
nix develop path:. --command bash -lc \
  "python -m unittest discover -s tests && cd src && dune build"
```

Generated output, caches, local environments, and build products are ignored by
Git.
