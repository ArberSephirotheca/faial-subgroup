# Subgroup Port Verification

Verified on 2026-09-11. This ports `integration/subgroup-upstream-20260713`
(`fc82f860`) onto upstream `main` (`24a363fc`) in the separate worktree
`artifact/faial-subgroup-upstream-port`, on
`integration/subgroup-upstream-20260911`. The original worktree and paper are
unchanged. Nothing was pushed.

## API Adaptations

- Use upstream's structured types, typed conversions, read-result expressions,
  formulas, access IDs, and pointer/location aliases.
- Preserve qualified function identities, declaration IDs, and template
  arguments during subgroup helper resolution and kernel selection.
- Preserve folded Boolean constants and `nullptr` when reading CUDA JSON.
- Share upstream's ordinary lowering pipeline, including helper bodies,
  custom rules, rejected-kernel reporting, and assumption validation.
- Adapt the CLI, reports, and Genie boundary to the new analysis APIs. Genie
  remains an ordinary-kernel tool.
- Retain per-site subgroup memory events. Loop-aware barrier alignment
  supplements these events; shuffles do not order memory. No new matrix
  memory checker was added.

Kernel-scoped assumptions now use `--assume 'kernel=NAME: CONDITION'`.
Binder-scoped assumptions still work for ordinary kernels, but are rejected
for subgroup kernels rather than silently promoted to kernel preconditions.
Subgroup analysis also rejects ordinary-pipeline `--stop-at` and Python/Z3
export requests explicitly.

## Build and Tests

| Check | Result |
| --- | --- |
| `dune build @all` | Pass |
| Unit tests in 65 suites | 1,034 pass; 23 optional archive cases skipped |
| DRF CLI examples | 427 of 443 pass |
| Remaining 16 CLI examples | Same failures on unmodified upstream |
| Whitespace and merge-conflict checks | Pass |

The skipped tests need the optional historical launch manifest at
`agent_results/rewrite/cuda_launch_manifest.json`. Their guard tests pass.
New regressions cover qualified/template helper identities, folded Boolean
constants, typed conversions and read-result uniformity, ordinary helper-body
preservation, and assumption/CLI boundaries. The nine subgroup memory-ordering
regressions pass.

The CLI failures include inherited methods, member/template calls, record
accesses, and launch-site lambdas. None is specific to `--subgroup-size`.
They must not be reported as passing tests: the installed archived translator
does not provide everything the current upstream pipeline expects. An isolated
rebuild of `c-to-json` with LLVM 20.1.8 completed, but the resulting executable
segfaulted on the CUDA example inputs. It was not installed over the existing
toolchain. End-to-end Genie/CBOR validation remains unavailable with this
translator; Genie builds and its unit tests pass.

From the port worktree, the local build and unit-test commands were:

```sh
opam exec --switch=../faial-subgroup -- dune build @all
PATH="$(pwd)/../toolchain-json/bin:$PATH" opam exec --switch=../faial-subgroup -- \
  dune runtest --force drf/test inference/test protocols/test imp/test \
  stage0/test named_barrier_div/test barrier_div/test drf/genie/test \
  ra/test delin/test rel_cost/test test
```

CLI cases are enumerated in `examples/drf/test.ml`. They were also run
individually with a 90-second process timeout, replaying failing ordinary
cases against unmodified upstream. The machine-readable summary lists all
16 failures and both exit codes.

## llama.cpp Replay

Both the port and unmodified upstream were run on all 479 archived manifest
entries, using eight workers and a 60-second timeout per entry. Assumption
syntax was adapted, without changing the facts. Source, configuration,
manifest, translator, and executable hashes were recorded. No orchestration
errors occurred.

The archived manifest cannot be treated as a current upstream manifest:
launch deduplication and qualified names have changed, and upstream now
rejects some previously accepted inputs. A missing kernel, rejected kernel,
zero-access result, or vacuous result is not a DRF pass.

The full port replay produced:

| Outcome | Entries |
| --- | ---: |
| Ordinary DRF / racy | 69 / 35 |
| Ordinary zero-access / vacuous | 27 / 1 |
| Subgroup: memory pass, participation pass | 45 |
| Subgroup: memory alarm, participation pass | 15 |
| Rejected: unnamed region / undefined kernel | 39 / 4 |
| Missing archived kernel name | 131 |
| Invalid assumption | 1 |
| Subgroup extraction/control error | 10 |
| Timeout | 102 |
| Total | 479 |

A later replay of all 98 formerly warp-routed entries obtained 60 conclusive
component results: 45 pass both checks and 15 report memory alarms with
participation passing. **All 60 component verdicts agree with the archive.**
The other 38 comprise 17 unnamed-region rejections, two undefined-kernel
rejections, one invalid assumption, and 18 timeouts. The invalid assumption
references `warp_size`, which is unbound in the new extracted protocol for
`gated_delta_net_cuda@gated_delta_net_192`.

Every ordinary DRF/racy result that completed in both full sweeps agreed.
A final targeted replay used the final binary for the 19 differing ordinary
entries and both group-normalization variants. Six earlier port timeouts
completed and matched upstream. Ten subgroup extraction/control errors and
three missing archived names remained; upstream had timed out on those three
names. Eight of the ten errors already occur in the old archive. The other
two are softmax kernels rejected for nonuniform breaks after helper expansion.
No successful ordinary DRF/racy verdict reversed relative to upstream in
these comparisons.

The final targeted run confirms that `group_norm_f32@norm_297` passes both
checks, while `group_norm_f32@norm_300` retains its memory alarm and passes
participation. These are separate runs, not adjustments to the full-sweep
counts above.

## Artifacts and Remaining Work

The adjacent `llama.cpp-eval/verification` directory contains:

- `upstream-port-2026-09-11-run5`: full port replay and ordinary/upstream comparison.
- `upstream-port-2026-09-11-warp-final`: subsequent 98-entry subgroup replay.
- `upstream-port-2026-09-11-upstream`: full unmodified upstream replay.
- `upstream-port-2026-09-11-final-targeted`: final 21-entry replay.
- `check_upstream_port.py`: repeatable runner with explicit outcome accounting.
- `upstream-port-2026-09-11-test-logs`: unit and CLI results.

Each corpus directory includes raw reports, a binary snapshot, provenance,
and comparison results. The full sweep preceded the last helper-identity
adjustments; the 98-entry replay followed those changes. Final unit/CLI tests
and the targeted replay followed the shared-lowering cleanup and CLI guards.

Before publishing replacement corpus totals, obtain a working translator for
the current upstream frontend, regenerate and validate the manifest names,
repair stale assumptions, and resolve or explicitly exclude unsupported
constructs. This port does not establish whole-corpus parity with the old
paper results. See `upstream-port-verification.json` for the generated summary.
