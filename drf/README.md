# DRF analysis

This directory contains the original Faial memory checker and the WarpDRF
subgroup extension. The public entry point is `bin/main.ml`; routing and
analysis orchestration live in `bin/app.ml`.

## Implementation map

| Module | Responsibility |
| --- | --- |
| `lib/wellformed.ml`, `lib/phasealign.ml` | Classify control flow and align barrier-containing loops. |
| `lib/phasesplit.ml`, `lib/locsplit.ml`, `lib/flatacc.ml` | Split ordinary protocols into phases, arrays, and conditional accesses. |
| `lib/symbexp.ml` | Encode ordinary two-thread memory obligations. |
| `lib/memory_event.ml` | Adapt subgroup source events and construct per-site memory obligations. |
| `lib/subgroup_solver.ml` | Solve subgroup memory obligations and classify results. |
| `lib/subgroup_uniformity.ml` | Check structural warp-uniformity of primitive control conditions. |
| `lib/subgroup_uniformity_solver.ml` | Prove additional uniformity facts from symbolic aliases and launch constraints. |
| `lib/subgroup_delinearize.ml` | Apply optional array-index simplification to subgroup memory effects. |
| `bin/analysis.ml` | Combine ordinary and subgroup results for CLI reporting. |

Source extraction and helper resolution belong to
[`inference`](../inference/README.md), not the solver. Shared expression
operations belong to `Protocols.Exp`.

## Routing

Without `--subgroup-size`, CUDA follows the original Faial pipeline. With an
explicit subgroup size, `Subgroup_source` identifies kernels containing
supported warp or matrix operations. Those kernels use the subgroup checker.
Other kernels use `Protocol_parser.d_program_to_proto` on the complete source
program and then the canonical `Symbexp.translate` pipeline.

Ordinary routing must retain all reachable device-helper definitions. Lowering
an isolated entry point can omit the helper's memory accesses and incorrectly
prove DRF. Selecting one entry with `--kernel` must not discard its helper
context or make an unrelated unsupported entry abort the selected analysis.

The subgroup path keeps source locations, access guards, numeric aliases,
launch facts, and block/warp phases for each ordinary memory effect. Each
candidate conflict is checked using two distinct symbolic threads. Block phases
separate all threads in a block; warp phases separate only threads in the same
configured warp.

For repeated memory-ordering barrier sites, the CLI uses the original
loop-aware protocol analysis instead of identifying all iterations with one
static barrier site. Per-site memory events remain in the primary analysis;
the loop-aware result supplements it for accesses crossing repeated barriers.
It retains block-barrier alignment but does not add aligned warp-barrier
ordering, so repeated warp barriers can lead to extra alarms.
`--find-true-dr` does not filter this fallback's memory protocol.

## Verdicts and assumptions

The subgroup JSON/text report exposes three verdicts:

- `mem_drf`: the memory result under the extracted event and launch assumptions.
- `subgroup_uniformity`: the participation result for warp and matrix sites.
- `drf_full`: passes only when both components pass.

The CLI computes memory and participation separately. The memory solver does
not wait for participation to pass: it uses the extracted synchronization
groups and phases. A memory-only pass therefore is not a full WarpDRF proof
when participation fails.

Unsupported constructs, unknown solver results, and timeouts are not passes.
With `--check-pre-sat`, inconsistent launch assumptions produce a
`vacuous` result rather than a DRF proof. `--assume-delin` also enables this
precondition check.

The upstream assumption syntax is `--assume 'kernel=NAME: CONDITION'` for
kernel-specific preconditions, or `--assume 'CONDITION'` for all selected
kernels. The previous `NAME: CONDITION` spelling is no longer accepted.
Upstream binder-scoped assumptions are supported for ordinary kernels. They
are explicitly rejected for subgroup kernels: a loop-local fact cannot be
treated as a global source-event precondition.

Upstream also reports rejected kernels separately from analyzed kernels in
JSON. An empty `kernels` list with a nonempty `rejected` list is not a DRF
result. Kernel labels include namespaces and template arguments, and launch
deduplication may change names relative to an older generated manifest.
The ordinary pipeline's `--stop-at` stages and Python/Z3 export are not
available for subgroup kernels; these combinations report an explicit error.

`--assume-launch` links host launch wrappers before routing. Exact block
dimensions and assertions constrain both analyses. A symbolic launch tuple
must include all three positive block dimensions; partial or conflicting tuples
are rejected.

Source-uniform variables and memory globals serve different purposes. A value
can be equal within a warp but differ between warps. Only memory-global values
are shared between the two symbolic tasks; ordinary locals and warp-owned
values keep task-specific names.

## Launch contracts

`lib/launch_contract.ml` validates the explicit `--launch-contract` option
and injects its preconditions, launch dimensions, and integral template
parameters. `lib/launch_contract_generator.ml` owns the supported row facts
and validators; `lib/launch_contract_rows.ml` holds the seed catalog.

For example, row `L072` selects `gated_linear_attn_f32<64>`:

```bash
faial-drf --launch-contract L072 --kernel gated_linear_attn_f32 \
  --block-dim 64 -D__CUDACC__ path/to/gla.cu
```

Contracts supply facts to the ordinary or subgroup analysis; they do not
replace solver results with a row verdict. Unknown rows, mismatched kernels,
incompatible dimensions, and missing required facts are rejected. Profile
bounds alone are not accepted as exact launch preconditions. The authoritative
row set is in the generator and its tests rather than a duplicated README list.

`lib/symbolic_launch_evidence.ml` contains optional evaluation export hooks:
`FAIAL_S438_SYMBOLIC_OBLIGATION_OUT`, `FAIAL_S439_SYMBOLIC_PROOF_OUT`,
`FAIAL_S488_UNGUARDED_SYMBOLIC_PROOF_OUT`,
`FAIAL_S489_ALL_FAMILY_FRONTIER_OUT`, and
`FAIAL_S491_BLOCKER_RETIREMENT_OUT`. These retain compatibility with the
external evaluation archive and are not needed for normal analysis.

## Current boundaries to review

- WMMA load/store footprints remain in the source representation, but the
  current memory checker builds obligations only for ordinary source effects.
  Matrix participation is checked; matrix-tile memory coverage is not implied.
- The static participation checker checks full-warp use, not arbitrary
  policy-relative partial participation. Unresolved control and call shapes
  cannot be assumed uniform.

## Build and test

From the repository root with the local opam switch installed:

```bash
opam exec --switch=. -- dune build @all
opam exec --switch=. -- dune runtest drf/test inference/test protocols/test
opam exec --switch=. -- dune runtest
```

`test_subgroup_memory_ordering` checks the source-to-solver path: a shuffle,
reduction, or matrix collective must not hide an ordinary-memory race, while
warp and block barriers retain their respective ordering scopes. The CUDA
fixtures `examples/drf/racy-subgroup-shuffle.cu` and
`examples/drf/drf-subgroup-shuffle-syncwarp.cu` exercise the same distinction
through the CLI with `--subgroup-size 32 --block-dim 32`.

The focused suites exercise source routing, phase and obligation construction,
solver verdicts, uniformity, delinearization, and launch-contract validation.
The full suite also runs CUDA examples and requires a working `cu-to-json` on
`PATH`. The existing WGSL tests skip translator-dependent checks when their
translator is unavailable.

The launch-manifest integration suite requires an external archive containing
`agent_results/rewrite/cuda_launch_manifest.json` and its referenced evidence
files. It is explicit rather than part of the self-contained default tests:

```bash
FAIAL_LAUNCH_ARTIFACT_ROOT=/path/to/evaluation-root \
  opam exec --switch=. -- dune build @drf/test/runtest-launch-manifest
```

The integration suite checks row-derived counts, generated launch facts,
evidence paths, and rejected neighboring specializations. Missing archive
files remain errors; the suite does not silently skip them.
