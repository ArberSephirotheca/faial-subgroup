# DRF Analysis

Code specific to the DRF analysis.

- bin/tui.ml: Text UI widgets
- bin/main.ml: Faial v1.0
- bin/parse2.mly: parser used by Faial v1.0
- bin/scan.mll: lexer used by Faial v1.0
- bin/next_gen.ml: Faial v2.0
- bin/z3_solver.ml: SMT solving functionality used by Faial v2.0
- lib/typecheck.ml: Lightweight sanity checks for protocols
- lib/wellformed.ml: Step 1. converts proto.ml into well-formed terms (synchronized/unsynchronized loops/conditionals, etc)
- lib/phasealign.ml: Step 2. well-formed into aligned protocols
- lib/phasesplit.ml: Step 3. aligned-protocols into phase-split
- lib/locsplit.ml: Step 4. phase-split into location split (one phase-split per array)
- lib/flatacc.ml: Step 5. location-split into flat-acc: flattens control-flow flow into lists of conditional accesses
- lib/symbexp.ml: Step 6. flat-acc into boolean expressions
- lib/memory_event.ml: Internal DRF memory-event boundary. It adapts
  `Flatacc.Kernel.t` values into ordinary memory events for one
  already-split workgroup phase/location, then re-emits `Symbexp.Proof.t`
  obligations for parity tests. It also maps existing subgroup/matrix carrier
  records into unified event artifacts and owns the subgroup-aware
  event-to-obligation builder exposed as `Memory_event.Subgroup_obligation`.
- lib/gensmtlib2.ml: Step 7. boolean expressions into smtlib2 (Faial v1.0 only)

## Ordinary Memory-Event Boundary

`lib/memory_event.ml` is the first internal boundary for unifying ordinary and
subgroup/matrix memory obligations. For ordinary kernels, it consumes
`Flatacc.Kernel.t`, which means all original source lowering, workgroup phase
splitting, location splitting, and flat-access generation have already run.
The adapter records ordinary memory events with access id, phase id, access
mode, source guard, runtime condition, and source location from the access
array variable. It then builds the same ordinary proof shape as
`Symbexp.Proof.from_flat`: projected task choices, access-id ordering,
index equality, non-negative index constraints, mode-conflict rules, runtime
guards, preconditions, and access summaries.

This boundary now owns ordinary public proof construction for non-subgroup kernels:
`App.run` routes flat-access streams through `Memory_event.translate`, and
`--unreachable` routes through `Memory_event.sanity_check`. Downstream
`Symbexp` proof decoration, CLI text/JSON output, and solver behavior still
use the existing workgroup-oriented entry points. At this seam, workgroup
barriers are represented by the existing phase separation rather than by
stable barrier-site event records; tasks that need barrier-site identity must
move the event input earlier than `Flatacc`.

`lib/flatacc.ml` also owns the projection boundary for phase-level loop ranges
produced by phase splitting. Lifted range binders are local to the checked
thread execution, so Flatacc keeps those binders in the local-variable set and
attaches their `Range.to_cond` facts to each flattened access guard. The
ordinary symbolic proof then projects them as `t$T1` and `t$T2` instead of
using one shared symbolic induction variable. This preserves source loop
scoping; it is not a DRF proof rule by itself. Ownership reasoning for
specific shapes such as `base + threadIdx.x + k * stride` remains a separate,
guarded solver or pre-solver concern.

## Launch Contracts

`lib/launch_contract_rows.ml` owns the current row catalog for guarded
launch/template/shape contracts. `lib/launch_contract.ml` owns the validator
and proof-context injector for ordinary DRF runs whose production host launch
carries facts that are not available when the analyzer enters a parsed CUDA
kernel directly. A launch contract appends source preconditions to
`Protocols.Kernel.pre`, adds the needed scalar kernel parameters as global
variables, and merges required integer template parameters before the
ordinary MAP pipeline runs.

The current supported contracts are the ggml-cuda GLA rows and the first WKV
row:

- `L072`: `gated_linear_attn_f32<64>`
- `L073`: `gated_linear_attn_f32<128>`
- `L143`: `rwkv_wkv_f32<CUDA_WKV_BLOCK_SIZE>`

The GLA rows require `--kernel gated_linear_attn_f32` and supply `HEAD_SIZE`.
The WKV row requires `--kernel rwkv_wkv_f32` and supplies `block_size = 64`
from `CUDA_WKV_BLOCK_SIZE`. Each contract checks that any explicit
`--block-dim` matches the row, keeps `gridDim` symbolic, and adds the
row-local facts:

```text
HEAD_SIZE = 64 or 128, or block_size = 64
blockDim.x = template value
blockDim.y = 1
blockDim.z = 1
C / H = template value
B > 0
T > 0
C > 0
H > 0
gridDim.x = B * H
gridDim.y = 1
gridDim.z = 1
```

Example:

```bash
faial-drf --launch-contract L072 --kernel gated_linear_attn_f32 \
  --cu-to-json=./bin/cu-to-json --block-dim 64 \
  -D__CUDACC__ path/to/gla.cu
```

This is not a solver shortcut and it does not discharge a race by row name.
The ordinary phase split, flat access, memory-event generation, symbolic
obligations, and Z3 classification are unchanged; the solver simply receives
the same launch-side assumptions that selected the manifest row. Unsupported
rows, missing `--kernel`, conflicting `HEAD_SIZE`, conflicting `--block-dim`,
concrete `--grid-dim`, `--all-dims`, and subgroup/matrix kernels fail closed.
Adding a new row should extend the row catalog and the corresponding family
validator rather than adding solver-specific exceptions.

`drf/test/test_launch_contract_manifest.ml` keeps this boundary tied to the
canonical launch manifest. It verifies that every ordinary launch-contract
catalog row has exactly one matching manifest row, that manifest row facts and
artifact paths agree with the row-local JSON summaries, that derived manifest
counts match the rows, and that neighboring rows such as `L144`-`L146` remain
unpromoted until they receive their own exact evidence.

## Subgroup/Matrix Extension Boundary

- `lib/memory_event.ml` exposes an internal `Subgroup_event` adapter from
  `Inference.Subgroup_source.subgroup_kernel` into unified event artifacts.
  The adapter carries the explicit target configuration, memory globals,
  uniform variables, source-order ordinals, site-control conditions,
  memory-control conditions, workgroup phase, subgroup phase, matrix site id,
  and matrix footprint metadata. Matrix load/store memory events keep their
  original rectangular footprint and indexed access; ordinary source memory
  effects keep their source/runtime conditions and recorded phase.
- `Subgroup_event` remains the unified event stream. `Subgroup_obligation`
  consumes that stream to build subgroup-aware memory obligations, preserving
  solver-facing metadata and error taxonomy. Missing explicit subgroup target
  configuration and ordinary-effect target-config mismatches fail at the
  event-adapter boundary instead of assuming a lane mapping.
- `Memory_event.Subgroup_obligation` is the direct owner of subgroup/matrix
  memory obligations. The public subgroup analyzer route and solver-facing
  aliases call it directly when solving subgroup/matrix kernels; there is no
  active compatibility wrapper in the DRF library.
- The unified subgroup memory builder models matrix load/store effects and
  ordinary source memory effects reported by `Inference.Subgroup_source`.
  Ordinary effects are converted into subgroup-aware memory obligations with
  their recorded access mode, base/index expression, source guard,
  source-order ordinal, runtime condition, workgroup phase, subgroup phase,
  and target configuration. They are not silently dropped.
- Workgroup barriers split workgroup memory phases. Subgroup barriers,
  subgroup collectives, CUDA warp helper/shuffle collectives, and WMMA matrix
  collectives advance only the subgroup phase.
- Matrix load/store effects are consumed through the rectangular footprint API:
  `memory_effect_footprint`, `indexed_access`, and `bounds_condition`. The DRF
  boundary must not collapse matrix footprints to a scalar base pointer.
- Matrix memory obligations also consume the source bridge's per-site
  memory-control metadata when it is present. That metadata is intentionally
  separate from uniformity control: uniformity uses source participation
  guards, while memory DRF needs the aliases and loop facts that make the
  matrix footprint index precise.
- Subgroup ordering suppresses a candidate memory race only for same-subgroup
  invocations whose accesses are separated by subgroup phase, or whose matrix
  effects are attached to the same matrix collective site. Different subgroups
  remain solver-visible.
- Ordinary source memory effects before and after focused warp helper/shuffle
  collectives, such as `warp_max`, `warp_sum`, `warp_reduce_max`,
  `warp_reduce_sum`, `__shfl_sync`, and `__shfl_down_sync`, use that same
  rule: helper calls do not split workgroup phases and do not introduce memory
  effects, but they do provide subgroup phase boundaries for same-subgroup
  ordering. The `warp_reduce_*` aliases summarize reviewed helper calls; they
  do not infer subgroup size from `WARP_SIZE`, and direct width-sensitive
  `__shfl_xor_sync` modeling remains unsupported until guarded separately.
- Source-order ordinals are carried to the obligation boundary as evidence,
  but they are not by themselves a workgroup barrier or a cross-subgroup
  ordering rule.
- Ordinary and matrix memory obligations project scalar facts per task unless
  `Inference.Subgroup_source` marks the variable as a memory global. Memory
  globals cover task-invariant arithmetic such as kernel parameters, member
  fields rooted in those parameters, CUDA block/grid dimensions, and locals
  derived only from them. Those variables stay unsuffixed in the projected
  solver goal, while subgroup-owned row/lane locals remain suffixed as
  `$T1`/`$T2`. This keeps shared row-stride and base-offset facts available to
  Z3 without turning per-invocation ownership facts into globals.
- If subgroup phase reasoning needs subgroup identity, the caller must provide
  an explicit target configuration and the checked block dimensions. Missing
  configuration or missing block dimensions are unsupported boundaries, not
  assumed warp size or unbounded-thread fallbacks.
- Subgroup-aware memory obligations constrain both projected tasks to the
  checked invocation domain:
  `0 <= threadIdx.{x,y,z}$Tn < blockDim.{x,y,z}`, plus concrete
  `blockDim.{x,y,z}` facts for the checked kernel configuration. When checked
  block dimensions are supplied, these facts are included for every generated
  obligation, including ordinary same-phase obligations that do not otherwise
  need subgroup identity.
- `lib/subgroup_uniformity.ml` checks the subgroup/matrix carrier's subgroup
  operation sites separately from memory DRF. Top-level sites are accepted;
  supplied control facts from the source bridge, including `if`/`else` branch
  guards, are checked before user-facing verdicts are rendered. Guards that
  depend on subgroup-varying state such as `threadIdx.x`,
  `threadIdx.x % subgroup_size`, unknown locals, lane aliases, or subgroup
  collective results are reported as
  `subgroup_uniformity: undefined_behavior`.
- `threadIdx.x / subgroup_size` is accepted as a subgroup-uniform control term
  only when the explicit CUDA x-contiguous target configuration supplies the
  same subgroup size. `threadIdx.y` and `threadIdx.z` are treated as
  subgroup-uniform only after that explicit lane mapping is present. Missing
  configuration rejects thread-coordinate control or reports the target-config
  error; it is not an assumed warp size or lane mapping.
- Source metadata may also mark local aliases of the configured subgroup-id
  expression as uniform. The current accepted CUDA form is syntactic: a
  thread-x coordinate alias such as `tid = threadIdx.x`, followed by
  `warp_id = tid / subgroup_size`, where the divisor equals the explicit
  target subgroup size. The source bridge canonicalizes real parsed
  `threadIdx.x` member expressions before this check. It does not prove ranges
  or accept lane aliases such as `tid % subgroup_size`. These source-uniform
  facts are assignment-sensitive, not monotonic: a later scalar assignment
  replaces the facts for that local, and an assignment under subgroup-varying
  or unknown source control clears the fact unless the assignment remains
  syntactically subgroup-uniform. At branch and loop exits, scalar facts for
  later statements survive only when every reachable outgoing path preserves
  the same uniform/thread-coordinate/constant fact or value; this applies to
  facts introduced inside the construct as well as facts present at entry.
  Source-uniform variables visible at an already-collected subgroup or WMMA
  site are stored with that site's control metadata. For `if` statements, each
  branch records its site snapshots from the scalar facts visible at the
  branch entry, not from a sibling branch's exit facts. Mixed branches, loop
  bodies, or loop increments that introduce or restore a subgroup-id alias on
  only one path therefore keep later or sibling guarded subgroup operations at
  `subgroup_uniformity: undefined_behavior` without losing valid source-local
  controls collected inside the region.
- The internal composed verdict is `drf_full = mem_drf && subgroup_uniformity`.
  This is a library-level result boundary only; CLI output, structural
  pre-solver discharges, focused Flash Attention analyzer success, and
  arbitrary CUDA/Flash Attention support remain out of scope here.

## Solver Taxonomy And Evidence

- `lib/subgroup_solver.ml` consumes obligations whose types are owned directly
  by `Memory_event.Subgroup_obligation`; it records deterministic
  per-obligation evidence for the user-facing CLI. The evidence preserves the
  obligation id, workgroup phase, array, subgroup phases, symbolic goal, solver
  configuration, and Z3 version.
- Solver classifications remain distinct: `solver=unsat(drf)`,
  `solver=sat(racy)`, `solver=unknown(reason=...)`,
  `solver=timeout(reason=...)`, `unsupported(reason=...)`, and the reserved
  future `pre_solver=unsat(reason=...)` classification.
- Before handing a subgroup/matrix memory obligation to Z3,
  `lib/subgroup_solver.ml` substitutes unambiguous numeric constants proven by
  the obligation itself, folds the result, and then invokes the configured
  solver. This is formula normalization, not a pre-solver discharge: it does
  not emit `pre_solver=unsat(reason=...)` and it preserves the solver
  classification taxonomy.
- Unsupported subgroup/matrix memory boundaries, such as missing checked block
  dimensions for subgroup ordering or mismatched ordinary-effect target
  configuration, are reported as unsupported boundaries. They are not converted
  into solver unknown, timeout, racy, or DRF results.
- Subgroup uniformity remains a separate component. A memory-DRF solver report
  plus `subgroup_uniformity: undefined_behavior` composes to
  `drf_full: not_drf`; it is not a memory race or a solver failure.
- U513 reserved the pre-solver evidence class. Structural discharges may only
  emit `pre_solver=unsat(reason=...)` for one matched obligation and must leave
  no-match obligations solver-visible.

## Structural Pre-Solver Discharges

- `lib/subgroup_solver.ml` runs structural pre-solver checks before Z3 only for
  individual `Memory_event.Subgroup_obligation` obligations. A matching rule
  emits `pre_solver=unsat(reason=...)`; it does not certify a kernel by name.
- `lib/ordinary_solver.ml` applies the same narrow ownership discipline to the
  ordinary Faial proof path. It runs before the ordinary Z3 call and returns
  DRF for a proof only when every conflicting access-id pair in that proof
  matches a reviewed structural rule. If any pair does not match, the complete
  proof remains solver-visible.
- The contradictory path-condition filter discharges an obligation only when
  the projected left and right guards contain syntactic complements, such as a
  checked global condition `p <= 0` on one side and `p > 0` on the other, or
  when either projected guard is literally `false`. Task-local variables are
  projected separately for `T1` and `T2` unless the caller marks them global, so
  this rule does not infer cross-thread equality for ordinary locals.
- The one-dimensional strided thread ownership rule requires a concrete block
  dimension with `x > 0`, `y = 1`, and `z = 1`. The matched access index may be
  the owned loop/index variable directly or that variable plus or minus a
  shared offset, for example `t` or `t - C`. The two projected accesses must
  share the same owner variable base and the same projected offset; therefore
  `C` must be represented as a memory-global value for `t - C` to match. Both
  projected guards must contain the ownership fact
  `(owner - threadIdx.x) % blockDim.x == 0` or the equivalent checked concrete
  stride. Changed offsets, task-local offsets, multidimensional blocks, and
  missing stride facts remain solver-visible.
- The ordinary proof path carries each access guard/range condition in
  `Symbexp.AccessSummary` so structural rules inspect the same per-access facts
  that Z3 receives. This is a data-carrier change only; it does not weaken the
  ordinary symbolic goal.
- The hierarchical subgroup row/lane-vector ownership rule is parameterized
  but guarded. It requires a one-dimensional checked block, an explicit
  subgroup/lane ownership fact, lane-vector lower and upper bounds, a shared
  row-base alias whose stride is proven to be a multiple of the vector width,
  and an address equality whose component delta is not divisible by that
  vector width. The rule compares logical variable names after projection; it
  does not depend on source locations carried by parser variables.
- The WMMA tile row/lane ownership rule is also guarded and parameterized by
  the explicit target configuration. It requires a CUDA x-contiguous subgroup
  target, a one-dimensional checked block with `blockDim.x > 0`, and
  `blockDim.x` divisible by the configured subgroup size. From those facts it
  derives the number of subgroups in the block, then derives the tile-row
  count, tile-column count, head dimension, and head-block step from
  obligation-local matrix ownership facts rather than from fixed constants.
  The head-block step must equal `num_subgroups * tile_cols`, and the head
  dimension must be a positive multiple of that step. The matched access must
  still have the same logical shape: an index alias of
  `row * head_dim + head_block + elem`, an `elem_idx / tile_cols` and
  `elem_idx % tile_cols` decomposition, lane ownership of `elem_idx`, and
  subgroup-owned `head_block` bounds using the configured subgroup size. It
  discharges only the matched candidate pair; missing target configuration,
  missing element bounds, non-positive dimensions, incompatible head-step
  layout, changed address shape, or multidimensional blocks remain
  solver-visible.
- Subgroup-row ownership plus a fixed writer lane is not by itself a
  pre-solver proof. An any-lane reader and a lane-zero writer can still be
  distinct invocations at the same row unless a separately reviewed subgroup
  program-order rule applies.
- Lane-vector ownership is also not a proof when the obligation lacks
  row-stride side conditions that make different rows disjoint. Those cases
  remain solver-visible until the required row-stride and parameter bounds are
  represented as obligation-local facts.
- Changed path-condition shape, changed address shape, missing concrete block
  dimensions, multidimensional blocks, or invalid stride bounds do not
  discharge. Those obligations remain solver-visible under the U513 solver
  taxonomy.
- The GLA state-output proof shape with `i * HEAD_SIZE + threadIdx.x` and
  separate main-output/state-output regions is not part of the
  one-dimensional strided rule. It remains a solver-budget case unless a
  separately reviewed tile/region ownership rule is added.

## CLI Subgroup/Matrix Integration

- `faial-drf --subgroup-size=N` enables the explicit CUDA
  x-contiguous subgroup/matrix route for CUDA kernels whose source contains
  subgroup or WMMA operations. The public route now builds subgroup/matrix
  memory obligations through `Memory_event.Subgroup_obligation` before handing
  them to `Subgroup_solver`. Without this option, the ordinary non-WMMA
  source-to-`Imp` path remains the default, but CUDA subgroup/matrix source is
  rejected by `Subgroup_source` before ordinary `Imp` lowering so it cannot
  silently fall back to workgroup-only semantics.
- `--subgroup-size` validates `N` as a positive subgroup size and records the
  target as `cuda-like(threadIdx.x-contiguous(size=N))`. The model still does
  not infer warp size or lane mapping from the source.
- Subgroup memory ordering that needs subgroup identity also needs checked
  block dimensions. The focused Flash Attention command provides
  `--block-dim 64 --subgroup-size 32`; missing or invalid checked dimensions
  remain unsupported boundaries, not fallback assumptions.
- Text output for the subgroup path prints the component verdicts
  `mem_drf`, `subgroup_uniformity`, and `drf_full`, followed by deterministic
  solver/pre-solver evidence lines. JSON output includes the same components
  plus `target_config`, `memory_checks`, and per-obligation evidence.
- Source-level `if`/`else`, loop, switch, case, and default guards on subgroup
  or WMMA sites are passed into `lib/subgroup_uniformity.ml` before CLI
  rendering. A subgroup operation guarded by `threadIdx.x` or
  `threadIdx.x % subgroup_size` reports
  `subgroup_uniformity: undefined_behavior` and composes to
  `drf_full: not_drf`; it must not be reported as a top-level uniform site.
- The CLI subgroup route also passes source-uniform variables from
  `Subgroup_source`, including kernel parameters and syntactically uniform
  local or loop variables. This includes focused subgroup-id aliases derived
  from the configured CUDA x-contiguous `threadIdx.x / subgroup_size` mapping.
  Unknown locals, raw `threadIdx.x` aliases, stale aliases reassigned from
  subgroup-varying lane state, aliases restored by only one branch after
  another branch invalidated them, and aliases assigned under `threadIdx.x`
  dependent control still classify the guarded subgroup operation as
  `subgroup_uniformity: undefined_behavior`.
- Existing non-WMMA CLI behavior is intentionally preserved. For example,
  `faial-drf --cu-to-json=./bin/cu-to-json examples/drf/drf-saxpy.cu` still
  reports `Kernel 'saxpy' is DRF!` and does not require subgroup
  configuration.
- The focused Flash Attention analyzer path is not final Rust/OCaml semantic
  parity. The command reaches the subgroup pipeline, preserves the deterministic
  subgroup/matrix artifact, turns ordinary shared/global source effects into
  memory obligations beside WMMA effects, and feeds matrix-site memory controls
  into matrix obligations. For the current focused Flash-Attention
  configuration, matrix store self-pairs discharge, guarded WMMA tile pairs and
  lane-vector cross-component pairs discharge structurally before Z3, and no
  SAT/racy memory obligations remain at the 1000ms solver budget. The WMMA
  structural rule is no longer fixed to the current kernel's literal
  `64/32/16/64` constants, but it is still a shape-guarded proof over explicit
  CUDA x-contiguous target configuration and obligation-local matrix metadata.
  The historical W509 boundary included the `row_max_shmem` same-subgroup
  source-order residual and `dst` arithmetic ownership cases; W510 closed
  `row_max_shmem` by same-subgroup subgroup-phase ordering, W512 carried the
  `dst` lane-vector arithmetic/range facts, and the current pre-solver closes
  the focused solver-budget residual without changing the original Faial memory
  obligation semantics. This is not arbitrary CUDA, arbitrary Flash Attention
  support, full Rust/OCaml semantic parity, or a structured Rust/OCaml memory
  artifact parity claim.

## Current Focused Evidence

The checked focused command uses `flash_attn_wmma_mirror_kernel` from
`../flash_attn_wgsl_matrix_mirror.cu` with `-DFAIAL_CAPTURE`,
`-D__float2half_rn(x)=((half)(x))`, `--ignore-asserts`, `-t 1000`,
`--block-dim 64`, `--subgroup-size 32`, and `--find-true-dr`.

Current OCaml evidence:

```text
mem_drf: drf
memory_checks: 28 total, 0 racy, 0 unknown, 0 timeout, 0 unsupported, 8 pre_solver_unsat
subgroup_uniformity: drf
drf_full: drf
```

The eight structural discharges are `o_shmem` obligations `#16` and `#17` by
WMMA tile row/lane ownership, plus `dst` cross-component obligations `#19`,
`#20`, `#21`, `#23`, `#24`, and `#26` by hierarchical subgroup
row/lane-vector ownership. Same-component `dst` pairs remain solver-visible.

Live Rust oracle evidence for the same source/configuration:

```text
mem_drf: drf
subgroup_uniformity: drf
drf_full: drf
memory_checks: 774 total, 0 racy
```

The previous focused OCaml memory boundary was solver-budget sensitivity, not
a SAT race model: `o_shmem` obligations `#16` and `#17` and `dst` obligation
`#26` needed a larger Z3 budget before the guarded pre-solver rules. The
current focused result is DRF at the 1000ms budget because those obligations
are discharged by obligation-local structural proofs before Z3. The result is
still not arbitrary CUDA, arbitrary Flash Attention support, or full Rust/OCaml
structured memory artifact parity.

Ordinary non-WMMA behavior remains separate from that focused extension
boundary. The W515 regression evidence reran `opam exec -- make`,
`PATH="$PWD/bin:$PATH" opam exec -- dune runtest`, a no-`--subgroup-size`
`drf-saxpy.cu` DRF smoke, and no-`--subgroup-size` racy smokes for
`racy-saxpy.cu` and `racy-shared-scalar.cu`.
