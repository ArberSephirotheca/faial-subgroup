#!/usr/bin/env bash
# Test whole-binary determinism: run faial-genie --seed N --json twice on
# the same input, normalise timing-dependent fields, diff the output.
#
# If this fails, faial-genie's verdict (or anything else in genie_stats /
# verdict / assumes) depends on something other than the SMT seed. That's
# the strongest signal we have of OCaml-side nondeterminism reaching the
# user-visible output.
#
# Timing fields stripped before diff:
#   - phase_times.*       (wall clocks)
#   - argv                (contains the absolute fixture path)
#
# Exits non-zero on a mismatch with a unified diff printed to stderr.

set -u
set -o pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
FIXTURES=(
  "examples/drf/drf-saxpy.cu"
  "examples/drf/racy-saxpy.cu"
)
SEED="${SEED:-0}"
RUNS="${RUNS:-2}"

cd "$REPO_ROOT"

# Normaliser strips fields that legitimately vary across runs (timing
# captured by Phase_timer and the argv that includes our cwd-dependent
# tempfile or absolute paths).
NORMALISE='
import json, sys
d = json.load(sys.stdin)
d.pop("phase_times", None)
d.pop("argv", None)
print(json.dumps(d, sort_keys=True, indent=2))
'

fail=0

for fixture in "${FIXTURES[@]}"; do
  echo "=== $fixture (seed=$SEED) ==="
  tmpdir=$(mktemp -d)
  for i in $(seq 1 "$RUNS"); do
    dune exec --root . -- faial-genie --json --seed "$SEED" "$fixture" \
      2>/dev/null \
      | python3 -c "$NORMALISE" > "$tmpdir/run_$i.json"
  done
  if diff -q "$tmpdir/run_1.json" "$tmpdir/run_$RUNS.json" >/dev/null; then
    echo "  OK: $RUNS runs produced byte-identical normalised JSON"
  else
    echo "  FAIL: runs diverge under fixed seed" >&2
    echo "  diff (unified):" >&2
    diff -u "$tmpdir/run_1.json" "$tmpdir/run_$RUNS.json" >&2 || true
    fail=1
  fi
  rm -rf "$tmpdir"
done

if [ "$fail" -ne 0 ]; then
  echo
  echo "Determinism check failed. Implies OCaml-side or Z3-internal" >&2
  echo "nondeterminism survives the --seed flag on at least one fixture." >&2
  exit 1
fi

echo
echo "All fixtures deterministic under --seed $SEED."
