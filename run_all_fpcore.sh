#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PYTHON_BIN="${MINITUNER_PYTHON:-python3}"
exec "$PYTHON_BIN" "$ROOT/src/scripts/run_fpcore_suite.py" "$@"
