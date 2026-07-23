#!/usr/bin/env bash
set -euo pipefail

if [[ $# -lt 2 ]]; then
    echo "Usage: $0 \"<domain;expression>\" <name> [options]" >&2
    exit 2
fi

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
EXPRESSION="$1"
NAME="$2"
shift 2

OUTPUT_ROOT="$ROOT/output/fpcore"
GUIDANCE_POINTS=4
TIMEOUT=120
Z3_JOBS=1
Z3_BANDS=auto
Z3_ENGINE=pareto
SEARCH_ENGINE=auto
VALIDATOR=auto
TARGET_SECONDS=10
RESUME=1
OPTUNER_COMPATIBLE=0
OPTUNER_SATIRE_COMPATIBLE=0
VALIDATION_JOBS=8
PLOT=1

while [[ $# -gt 0 ]]; do
    case "$1" in
        --output-root) OUTPUT_ROOT="$2"; shift 2 ;;
        --guidance-points) GUIDANCE_POINTS="$2"; shift 2 ;;
        --no-guidance) GUIDANCE_POINTS=0; shift ;;
        --timeout) TIMEOUT="$2"; shift 2 ;;
        --z3-jobs) Z3_JOBS="$2"; shift 2 ;;
        --z3-bands) Z3_BANDS="$2"; shift 2 ;;
        --z3-engine) Z3_ENGINE="$2"; shift 2 ;;
        --search-engine) SEARCH_ENGINE="$2"; shift 2 ;;
        --validator) VALIDATOR="$2"; shift 2 ;;
        --target-seconds) TARGET_SECONDS="$2"; shift 2 ;;
        --resume) RESUME=1; shift ;;
        --no-resume) RESUME=0; shift ;;
        --optuner-compatible) OPTUNER_COMPATIBLE=1; shift ;;
        --optuner-satire-compatible) OPTUNER_SATIRE_COMPATIBLE=1; shift ;;
        --validation-jobs) VALIDATION_JOBS="$2"; shift 2 ;;
        --no-plot) PLOT=0; shift ;;
        *) echo "Unknown option: $1" >&2; exit 2 ;;
    esac
done

if [[ $OPTUNER_COMPATIBLE -eq 1 && $OPTUNER_SATIRE_COMPATIBLE -eq 1 ]]; then
    echo "--optuner-compatible and --optuner-satire-compatible are mutually exclusive" >&2
    exit 2
fi
if [[ ( $OPTUNER_COMPATIBLE -eq 1 || $OPTUNER_SATIRE_COMPATIBLE -eq 1 ) && "$VALIDATOR" != auto ]]; then
    echo "--validator is available only in native mode; compatibility modes lock it" >&2
    exit 2
fi

PYTHON_BIN="${MINITUNER_PYTHON:-python3}"
ARGS=(
    --expression "$EXPRESSION"
    --prefix "$NAME"
    --output-root "$OUTPUT_ROOT"
    --guidance-points "$GUIDANCE_POINTS"
    --timeout "$TIMEOUT"
    --z3-jobs "$Z3_JOBS"
    --z3-bands "$Z3_BANDS"
    --z3-engine "$Z3_ENGINE"
    --search-engine "$SEARCH_ENGINE"
    --validator "$VALIDATOR"
    --target-seconds "$TARGET_SECONDS"
    --validation-jobs "$VALIDATION_JOBS"
)
[[ $RESUME -eq 1 ]] && ARGS+=(--resume) || ARGS+=(--no-resume)
[[ $OPTUNER_COMPATIBLE -eq 1 ]] && ARGS+=(--optuner-compatible)
[[ $OPTUNER_SATIRE_COMPATIBLE -eq 1 ]] && ARGS+=(--optuner-satire-compatible)

"$PYTHON_BIN" "$ROOT/src/scripts/run_z3_bands.py" "${ARGS[@]}"
FRONTIER="$OUTPUT_ROOT/frontiers/$NAME.csv"
IMAGE="$OUTPUT_ROOT/images/$NAME.png"
if [[ ( $OPTUNER_COMPATIBLE -eq 1 || $OPTUNER_SATIRE_COMPATIBLE -eq 1 ) ]] && [[ $(wc -l < "$FRONTIER") -le 1 ]]; then
    echo "OpTuner-compatible run produced no certified candidate solutions" >&2
    exit 1
fi
if [[ $PLOT -eq 1 ]]; then
    "$PYTHON_BIN" "$ROOT/src/scripts/plot.py" "$FRONTIER" "$IMAGE"
fi
echo "Frontier: $FRONTIER"
if [[ $PLOT -eq 1 ]]; then
    echo "Image: $IMAGE"
fi
