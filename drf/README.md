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
array variable. Ordinary proof construction delegates to the canonical
`Symbexp.Proof.from_flat` encoder rather than copying its rules. This preserves
the original MAP task projection, conflict rules, atomic axioms, selected
memory-model assumptions, preconditions, and access summaries as upstream
Faial evolves.

This boundary does not replace ordinary public proof construction. `App.run`
routes ordinary flat-access streams directly through canonical
`Symbexp.translate`; `Memory_event.translate` remains an internal parity and
extension adapter. At this seam, workgroup
barriers are represented by the existing phase separation rather than by
stable barrier-site event records; tasks that need barrier-site identity must
move the event input earlier than `Flatacc`.

Mixed ordinary/subgroup parsing assigns collision-free names to global
`D_lang` entries after launch-wrapper linking and before selected subgroup
lowering. The same suffix policy remains in place after compilation, so a
token such as `kernel_2` selects the same source-order entry on replay.
Auxiliary definitions remain link context but are not independently analyzed,
and an unsupported unselected entry cannot abort the selected route.

`lib/flatacc.ml` keeps upstream local-variable classification. The subgroup
extension does not reinterpret ordinary MAP loop binders. Ownership reasoning
for recognized shapes such as `base + threadIdx.x + k * stride` remains a
separate guarded pre-solver concern and is enabled only by an explicit launch
contract.

## Subgroup Launch Domains

For a linked `--assume-launch` wrapper, `Subgroup_source` carries top-level
launch assertions into every source memory effect and records exact
`blockDim.{x,y,z}` equalities separately. `Memory_event.Subgroup_obligation`
turns a complete three-axis tuple into checked symbolic dimensions: each task
coordinate is bounded by the launch value and every symbolic dimension has an
explicit positive guard. A partial tuple or conflicting dimension fact fails
instead of falling back to unconstrained dimensions.

When `--check-pre-sat` is enabled (including when `--assume-delin` forces it),
the subgroup CLI checks the conjunction of the linked launch precondition,
dimension equalities, and positivity guards. An unsatisfiable conjunction is
reported as structured `status=vacuous` / `drf_full=vacuous`, never as a DRF
proof. This pre-check is separate from per-access race obligations.

## Launch Contracts

`lib/launch_contract_rows.ml` owns the current row catalog for guarded
launch/template/shape contracts. `lib/launch_contract.ml` owns the validator
and proof-context injector for ordinary DRF runs whose production host launch
carries facts that are not available when the analyzer enters a parsed CUDA
kernel directly. A launch contract appends source preconditions to
`Protocols.Kernel.pre`, adds the needed scalar kernel parameters as global
variables, and merges required integer template parameters before the
ordinary MAP pipeline runs.

Launch-shape behavior is data-driven through
`Launch_contract_generator.shape_contract`. The generator owns the row facts:
which integer globals must be injected, which symbolic precondition facts hold,
and whether the row requires an explicit subgroup route. `lib/launch_contract.ml`
interprets those facts into MAP expressions and fail-closed option checks; it
does not branch on a row family to decide the precondition or subgroup route.
Executable lookup/catalog rows are also generator-owned
`Launch_contract_generator.launch_contract_row` records. `Launch_contract_rows.t`
is kept as a seed/manifest-compatibility representation for the generated
GLA/WKV/WKV7 catalog only; `lib/launch_contract.ml` does not adapt
`Launch_contract_rows.t` directly.

The current executable launch-contract rows include the ggml-cuda GLA rows,
the `L012` clamp finite type-template row, the conv/cpy profile rows, the
q8/regular dequantize block profile rows, the first exact fill profile
consumption rows, the im2col bounded symbolic blockDim rows, the first S483
profile-backed scalar/unary rows, the two selected solve-tri subgroup rows, the
first two WKV6 rows, and the first two WKV7 rows:

Production-profile launch facts pass through a separate context boundary before
they become executable rows. `Launch_contract_generator.profile_launch_context`
classifies each row-owned fact as `Fixed_by_launch`,
`Fixed_by_model_or_template`, `User_symbolic`, `Derived`, `Equal_to`,
`Profile_bounded`, or `Unknown_blocker`. Exact launch-contract materialization
is admitted only when no role is `Profile_bounded` or `Unknown_blocker`. This
keeps model/template-fixed facts, user-controlled positive dimensions, derived
launch expressions, and equality facts explicit while preventing an observed
profile bound from silently becoming a proof precondition. The current admitted
context-backed rows are `L012`, `L018`, `L019`, `L020`, `L021`, `L024`,
`L025`, `L026`, `L027`, `L028`, `L029`, `L030`, `L031`, `L032`, `L033`,
`L034`, `L035`, `L036`, `L037`, `L038`, `L039`, `L040`, `L041`, `L042`,
`L043`, `L046`, `L047`, `L048`, `L049`, `L050`, `L051`, `L052`, `L053`,
`L054`, `L055`, `L056`, `L067`, `L068`, `L076`, `L135`, `L136`, `L137`, and
`L138`. The im2col bounded symbolic blockDim lookup rows are `L074` and
`L075`. Their soundness boundary remains production-backed DRF ownership
extraction or profile evidence, not full host translation-unit parsing and not
numeric equivalence.

- `L072`: `gated_linear_attn_f32<64>`
- `L073`: `gated_linear_attn_f32<128>`
- `L012`: `op_clamp_kernel<T>`, with finite type domain `T in {half,float}`
- `L018`: `conv2d_dw_kernel<float, whcn_layout>`
- `L019`: `conv2d_dw_kernel<float, cwhn_layout>`
- `L020`: `conv2d_transpose_kernel<half>`
- `L021`: `conv2d_transpose_kernel<float>`
- `L024`: `dequantize_block_q8_0_f16<need_check>`, false branch
- `L025`: `dequantize_block_q8_0_f16<need_check>`, true branch
- `L026`-`L043`: exact dequantize block profile rows from `convert.cu`
- `L046`: `cpy_f32_q<cpy_blck_f32_q8_0, QK8_0>`
- `L047`: `cpy_q_f32<cpy_blck_q8_0_f32, QK8_0>`
- `L048`: `cpy_f32_q<cpy_blck_f32_q4_0, QK4_0>`
- `L049`: `cpy_q_f32<cpy_blck_q_f32<dequantize_q4_0, QK4_0>, QK4_0>`
- `L050`: `cpy_f32_q<cpy_blck_f32_q4_1, QK4_1>`
- `L051`: `cpy_q_f32<cpy_blck_q_f32<dequantize_q4_1, QK4_1>, QK4_1>`
- `L052`: `cpy_f32_q<cpy_blck_f32_q5_0, QK5_0>`
- `L053`: `cpy_q_f32<cpy_blck_q_f32<dequantize_q5_0, QK5_0>, QK5_0>`
- `L054`: `cpy_f32_q<cpy_blck_f32_q5_1, QK5_1>`
- `L055`: `cpy_q_f32<cpy_blck_q_f32<dequantize_q5_1, QK5_1>, QK5_1>`
- `L056`: `cpy_f32_q<cpy_blck_f32_iq4_nl, QK4_NL>`
- `L067`: `fill_kernel<T>`, F32 dispatch, with finite type domain `T=float`
- `L068`: `fill_kernel<T>`, F16 dispatch, with finite type domain `T=half`
- `L074`: `im2col_kernel<T>`, bounded symbolic `blockDim.x`
- `L075`: `im2col_3d_kernel<T>`, bounded symbolic `blockDim.x`
- `L076`: `divide_by_count<float>`, exact single-block/single-thread scalar row
- `L135`: `swiglu_oai_kernel<T>`, with finite type domain `T=float`
- `L136`: `xielu_kernel<T>`, with finite type domain `T in {half,float}`
- `L137`: `silu_back_kernel<T>`, with finite type domain `T in {half,float}`
- `L138`: `leaky_relu_kernel<T>`, with finite type domain `T in {half,float}`
- `L117`: `solve_tri_f32_fast<64, 32>`
- `L118`: `solve_tri_f32_fast<64, 16>`
- `L143`: `rwkv_wkv_f32<CUDA_WKV_BLOCK_SIZE>`
- `L144`: `rwkv_wkv_f32<CUDA_WKV_BLOCK_SIZE * 2>`
- `L145`: `rwkv_wkv7_f32<CUDA_WKV_BLOCK_SIZE>`
- `L146`: `rwkv_wkv7_f32<CUDA_WKV_BLOCK_SIZE * 2>`

`L012`, `L018`-`L021`, `L024`-`L043`, `L046`-`L056`, `L067`, `L068`, `L074`,
`L075`, `L076`, `L117`, `L118`, and `L135`-`L138` are different from the
ordinary GLA/WKV/WKV7 catalog rows. `L012`
is a finite type-template
lookup row: the launch contract records the finite host dispatch domain
`T in {half,float}`, the checked production-backed extraction kernel
`op_clamp_kernel_l012_float`, block dimension `[256,1,1]`, the symbolic grid
source `(k + 255) / 256`, and the positive guard `k > 0`. `L067` and `L068`
consume the S481 production-backed `fill_kernel` profile as exact executable
launch-contract rows. They both use `CUDA_FILL_BLOCK_SIZE = 256`, symbolic grid
source `(k + 255) / 256`, positive guard `k > 0`, and the checked DRF ownership
shape `write dst[i] under i < k`. The `L068` extraction abstracts element
bitwidth because this proof concerns address ownership and race freedom, not
numeric F16 equivalence. `L024` and `L025` consume the S481
`dequantize_block_q8_0_f16<need_check>` profile by resolving the branch-local
finite bool template facts `need_check=false` and `need_check=true`.
`L026`-`L043` consume the S481 regular dequantize block profiles whose launch
block size is concrete (`32` or `64`) and whose block/lane ownership writes one
destination lane per block. These rows verify address ownership only; they do
not claim numeric dequantization equivalence or full host-template parsing.
`L076` consumes the `divide_by_count<float>` profile
with exact launch shape `gridDim=[1,1,1]` and `blockDim=[1,1,1]`; this exact
grid fact is required because the profile reads and writes `result[0]`.
`L135`-`L138` consume one-dimensional unary S481 profiles with block dimension
`[256,1,1]`, symbolic positive `gridDim.x`, positive guard `k > 0`, and
per-thread destination ownership under `i < k`. `L117` and `L118` remain
solve-tri lookup rows routed through the subgroup/matrix analyzer under
explicit `--subgroup-size 32`, and their accepted evidence is row-local: H516
plus S404 for `L117`, and S408 plus S409 for `L118`. These lookup rows are not
ordinary
`Launch_contract.catalog_rows` entries.

The GLA rows require `--kernel gated_linear_attn_f32` and supply `HEAD_SIZE`.
The WKV6 rows require `--kernel rwkv_wkv_f32`; the WKV7 rows require
`--kernel rwkv_wkv7_f32`. They supply `block_size = 64` from
`CUDA_WKV_BLOCK_SIZE` for `L143` and `L145`, or `block_size = 128` from
`CUDA_WKV_BLOCK_SIZE * 2` for `L144` and `L146`. Each contract checks that
any explicit `--block-dim` matches the row, keeps `gridDim` symbolic, and
adds the row-local facts:

```text
HEAD_SIZE = 64 or 128, or block_size = 64 or 128
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
counts match the rows, and that neighboring rows remain unpromoted until they
receive their own exact evidence. It also treats the exact WKV row set
`L143`-`L146` as the closed H505+ continuation campaign and checks that each
launch-contract artifact key belongs only to its owning row.

`lib/launch_contract_generator.ml` owns the pure H510 family-generation seam
for the already accepted seed rows. It records GLA, WKV6, and WKV7 family
facts such as parsed kernel identity, template expression, source launch
branch selection, exact row-shape preconditions, block/grid sources, dynamic
shared memory, required semantics, evidence-artifact key, and timeout. The
source branch and row-shape facts are intentionally separate: wide rows record
that the source launch is the `else` branch, while the exact launch contract
still records concrete preconditions such as `C / H == 128` and
`blockDim.x == 128`. H511 switches the production launch-contract catalog for
the accepted GLA rows `L072` and `L073` to the generated GLA records. H512
switches the accepted WKV6 rows `L143` and `L144` and WKV7 rows `L145` and
`L146` to generated records as well, so the production catalog for the
accepted seed set is generator-backed while the manual catalog remains the
equivalence oracle. The generator remains admitted only for accepted seed rows
whose generated records match the manual row facts and canonical manifest.
This does not promote new rows or broaden GLA, WKV, WKV7, shared-memory,
subgroup/matrix, or arbitrary ggml-cuda support.

The generator also exposes a pure manifest-fact validation guard used by
`drf/test/test_launch_contract_manifest.ml`. The guard requires explicit
row-local facts for family classification, manifest and parsed kernel names,
template argument and concrete value, block/grid sources, source branch,
dynamic shared memory, evidence-artifact key, and timeout. Missing family
facts, ambiguous family classifiers, missing branch or block/grid facts,
missing dynamic-shared-memory facts, and missing evidence-artifact
expectations fail closed; the generator does not infer defaults from row IDs,
kernel names, filenames, expected output counts, or accepted seed shapes.

H517 adds a separate selected-row generated fact surface for `L117`
(`solve_tri_f32_fast<64, 32>`). It records the H516 source-intake facts for
the selected solve-tri row: source-slice kernel `solve_tri_f32_fast_l117`,
template bindings `n_template = 64` and `k_template = 32`, launch branch
conditions `n == 64` and `case 32`, concrete block dimension `[32, 32, 1]`,
dynamic shared memory `0`, source-slice artifact paths, `warp_reduce_sum` as
a subgroup helper, and explicit subgroup size `32`. These facts are guarded
by `validate_selected_manifest_facts` against the canonical manifest row plus
H516 artifacts. That guard is the pre-promotion admission check; after S404,
the canonical manifest row records the verified subgroup verdict while the
H516 selected facts remain the route evidence.

H520 admits those selected facts to the production `Launch_contract` lookup for
`--launch-contract L117` without changing the canonical manifest verdict. The
lookup row supplies the two template bindings, the concrete `[32, 32, 1]`
block shape, and row-local preconditions. It deliberately does not add `L117`
to the verified ordinary catalog rows, does not promote neighboring solve-tri
rows `L116` or `L118`-`L128`, and does not treat `warp_reduce_sum` as a
workgroup barrier.

S407 generalizes that selected-row lookup surface for the first concrete
neighbor, `L118: solve_tri_f32_fast<64, 16>`. Its row-local generated facts
require parsed kernel `solve_tri_f32_fast_l118`, template bindings
`n_template = 64` and `k_template = 16`, source branch conditions `n == 64`
and `case 16`, concrete block dimension `[32, 16, 1]`, dynamic shared memory
`0`, `warp_reduce_sum` as a subgroup helper, and explicit subgroup size `32`.
S408 supplies the verifier-facing source/profile artifact. S409 promotes only
canonical `L118` after exact structured subgroup JSON passes. Missing or
mismatched parsed kernel, template values, block shape, grid policy, evidence
artifact key, subgroup size, or any attempt to reuse `L117` facts fails closed.

S403 and S407 route validated solve-tri lookup rows through the existing
subgroup/matrix analyzer path, and only when the command provides explicit
`--subgroup-size 32`. Missing subgroup configuration, a different subgroup
size, a missing or mismatched `--kernel`, a mismatched `--block-dim`, concrete
`--grid-dim`, `--all-dims`, or any unselected solve-tri neighbor row still
fails closed. A same-named ordinary kernel is also rejected instead of falling
back to the ordinary launch-contract route.

S404 promotes only canonical `L117` in the launch manifest after the exact
production command exits successfully with structured subgroup JSON:

```bash
faial-drf --json --launch-contract L117 \
  --kernel solve_tri_f32_fast_l117 --block-dim '[32,32]' \
  --subgroup-size 32 -t 10000 \
  agent_results/rewrite/component_summaries/H516/artifacts/L117_solve_tri_f32_fast_l117_source_slice.cu
```

The accepted verdict is `status=drf`, `mem_drf=drf`,
`subgroup_uniformity=drf`, `drf_full=drf`, and two memory checks with zero
racy, unknown, timeout, or unsupported classifications. This does not promote
solve-tri neighbors `L116` or `L118`-`L128`, does not prove full-source
host-header intake for `solve_tri.cu`, and does not broaden shared-memory,
subgroup-helper, CUDA, or ggml-cuda support.

S447 adds a non-admission generic candidate carrier for the recurring S445
host/template blocker class. The carrier records that 54 families stopped at
`template_args_unresolved_or_conflicting` in the S445 sweep and lists the
required row-owned facts that must exist before source or proof work can run:
source file, kernel/template, concrete template arguments, launch site,
preprocessing profile, extraction fixture, include and macro profile,
block/grid sources, and dynamic shared memory. It is owned by
`Launch_contract_generator.guarded_candidate_carrier`, has
`solver_policy = not_solver_input`, and has
`admission_status = blocked_no_fresh_obligation`. It does not add a launch
lookup row, change manifest verdict fields, run a solver or pre-solver, or
turn first-blocker accounting into guarded family proof.

S454 validates populated row-owned carrier facts against the S447 required
fact-key/status schema. The current validated seed is the S453 `L012` clamp
family ledger; the validator checks the flattened carrier status entries for
source, launch, block/grid, dynamic-shared-memory, include, macro, and
profile-related facts, while source-visible memory effects and
alias/address-space assumptions remain explicit S455 proof-construction
blockers rather than S454-validated inputs. Partial or missing
`concrete_template_args`, `preprocessing_profile`, `extraction_fixture`,
`positive_shape_guards`, and alias/address-space assumptions keep the
readiness status at
`not_ready_missing_checked_specialization_profile_fixture_and_positive_guard`.
The validation surface is still pre-proof: it requires
`solver_policy = not_solver_input`, zero fresh obligations, zero solver and
pre-solver runs, zero guarded-family admissions, no lookup rows, and no
manifest verdict-field changes. It also requires
`shortcut_keying_used = false`, so row IDs, kernel names, task IDs, fixture
shapes, and expected output counts cannot stand in for row-owned facts.

S475 adds a typed template-argument resolution carrier for the S470
launch-template blocker class. It records the S475 artifact ledger, the S470
input blocker ledger, the exact row/family counts, and the resolution taxonomy:
48 of 79 rows and 33 of 54 families have known template-argument status; 31
rows and 21 families remain blocked by dependent or unresolved template
arguments. The carrier is exposed through `Launch_contract` and validated by
`validate_template_argument_resolution_carrier`, but it is still pre-proof
evidence. It does not emit memory events, generate obligations, run a solver or
pre-solver, admit guarded families, add lookup rows, change manifest verdict
fields, or promote any launch row. The next proof-input frontier must consume
the 33 S475-known families and move them to their next checked dependency,
while the 21 S475-blocked families remain fail-closed until their host-template
domains are extracted.

S477 adds the next typed launch-branch frontier carrier. It records that the
S477 frontier consumed the S476 ledger and inspected the 33
`launch_branch_status` families: 45 of 46 rows, covering 32 families, now have
row-local selected launch branch/profile evidence; the remaining `L013`
`common.cuh :: kernel` helper remains fail-closed because the launched callee is
an indirect `Kernel kernel` parameter. The carrier is exposed through
`Launch_contract.launch_branch_frontier_carrier` and validated by
`validate_launch_branch_frontier_carrier`. It is still pre-proof evidence: it
does not emit memory events, generate obligations, run solver work, admit
guarded families, add lookup rows, change manifest verdict fields, or promote
any row. The next proof-input task must consume the 32 branch-known families at
`positive_shape_guard_status` and keep the indirect launch helper blocked until
its specialization is extracted.

S478 adds the typed positive-shape guard frontier carrier for the next proof
input boundary. It consumes the S477 family frontier and records the 33
families / 57 rows currently blocked at `positive_shape_guard_status`: 32
families / 45 rows came from S477-selected launch branches, while the
preexisting `solve_tri_f32_fast` family contributes 12 rows from the guarded
family boundary. The carrier is exposed through
`Launch_contract.positive_shape_guard_carrier` and validated by
`validate_positive_shape_guard_carrier`. It owns the row/family set and the
required shape facts (`row_domain`, positive block/grid domains, dynamic shared
memory domain, zero-work exclusions, covered/excluded rows, source profile, and
memory-event list), but it is still pre-proof evidence. It does not emit memory
events, generate obligations, run solver or pre-solver work, admit guarded
families, add lookup rows, change manifest verdict fields, or promote any row.
The next proof-input task must derive executable positive block/grid guards and
zero-work exclusions per family before `Memory_event` or
`Subgroup_obligation` construction.

S479 records the verification sweep over those 33 positive-shape families as a
target-owned evidence carrier:
`Launch_contract.positive_shape_verification_carrier`. The sweep verifies all
33 attempted families at the current source-slice/guarded-family evidence
level: 32 families are verifier-facing generated source slices with
`faial_status = drf`, and the remaining family is the existing guarded
symbolic `solve_tri_f32_fast<N,K>` proof. The carrier records 57 attempted
rows, 32 source-slice artifacts, zero blocked families, zero exact production
row promotions, and no manifest verdict-field changes. This is real DRF
evidence for the S478 positive-shape frontier, but it is not a broad production
ggml-cuda claim: generated source slices still need row-owned extraction
profiles or exact launch-contract rows before they can become manifest
promotions.

S480 starts that production-backed promotion path with `L012`,
`clamp.cu :: op_clamp_kernel<T>`. The target-owned
`Launch_contract.positive_shape_production_promotion_carrier` records the real
source/launch facts: `CUDA_CLAMP_BLOCK_SIZE = 256`, block dimension
`[256,1,1]`, grid expression `(k + 255) / 256`, dynamic shared memory `0`,
finite host dispatch domain `T in {half,float}`, positive shape guard `k > 0`,
and the production memory effects `x[i]` read and `dst[i]` write under
`i < k`. The verifier-facing extraction
`L012_op_clamp_kernel_production_backed.cu` checks the representative
`T=float` memory behavior and returns DRF with no unknowns or errors. This
moves `F011/L012` from source-slice-only evidence to
production-backed-extraction evidence. The `--launch-contract L012` lookup row
now consumes that finite type-domain carrier data and injects the launch-shape
precondition for the checked extraction kernel. It still does not parse the
full ggml host translation unit or change manifest verdict fields.

S481 consumes the remaining S479 source-slice-only families and converts them to
production-backed DRF ownership profiles. The S481 ledger records 31 attempted
families / 44 rows, all profile-verified with fresh reruns that return DRF with
zero unknowns and zero errors. Each profile artifact binds the S479 source slice
to row-owned production facts from the CUDA launch manifest: `ggml-cuda` source
file, concrete launch row IDs, kernel/template name, launch site, block/grid
source, dynamic shared memory, and the checked memory-ownership shape. This
leaves zero S479 source-slice-only families. It still does not claim numeric
equivalence, full host-header parsing, exact manifest promotion, or new
`--launch-contract` rows for those 31 families. The production-backed evidence
frontier is therefore: one extraction family (`L012`), 31 production-profile
families, and the existing guarded symbolic solve-tri family.

S482 consumes the first S481 profile family into exact executable
launch-contract rows: `F050 fill.cu :: fill_kernel`, covering `L067` and
`L068`. Both rows verify with `--launch-contract` and return DRF with zero
unknowns and zero errors. This is stronger than S481 profile evidence because
the row-specific production profile is now connected to the executable launch
contract path. It still does not change manifest verdict fields or claim a full
host translation-unit parse.

S483 consumes five more S481 production-backed profiles into exact executable
launch-contract rows: `F057/L076 divide_by_count<float>`,
`F086/L135 swiglu_oai_kernel`, `F087/L136 xielu_kernel`,
`F088/L137 silu_back_kernel`, and `F089/L138 leaky_relu_kernel`. All five rows
verify with `--launch-contract` and return DRF with zero unknowns and zero
errors. `L076` uses exact grid and block facts for the single-thread scalar
profile; the unary rows use finite type domains only where F16 and F32 share the
same DRF address-ownership shape. S483 still does not change manifest verdict
fields, prove numeric equivalence, or parse the full host translation unit.

S485 consumes the regular S481 dequantize block profiles from `convert.cu` into
exact executable launch-contract rows `L026`-`L043`. These rows use the concrete
production block sizes recorded in the launch manifest (`32` or `64`), keep
`gridDim.x` symbolic and positive, add the positive block-count guard
`nblocks > 0`, and verify the source-slice ownership shape
`dst[block * blockDim.x + lane]` under `block < nblocks`. S485 does not consume
the remaining conv/copy/im2col profile families. S485 does not change manifest
verdict fields, does not prove numeric dequantization equivalence, and does not
parse the full host translation unit.

S486-A consumes the S481 `F020` q8 dequantize profile into exact executable
launch-contract rows `L024` and `L025`. The only additional launch/template
fact relative to the S485 regular dequantize rows is the branch-local finite
bool template domain: `L024` records `need_check=false` from the aligned branch,
and `L025` records `need_check=true` from the fallback branch. Both rows keep
`blockDim.x = WARP_SIZE = 32`, symbolic positive `gridDim.x = num_blocks`,
dynamic shared memory `0`, and the same block/lane ownership shape. S486-A
does not change manifest verdict fields, does not prove numeric q8
dequantization equivalence, and does not parse the full host translation unit.

S486-B consumes the S481 conv/cpy profile blockers into exact executable
launch-contract rows. The conv rows are `F016/L018-L019` for
`conv2d_dw_kernel<float, layout>` and `F017/L020-L021` for
`conv2d_transpose_kernel<T>`. The cpy rows are `F041/L046,L048,L050,L052,L054,L056`
for `cpy_f32_q<helper,QK>` and `F042/L047,L049,L051,L053,L055` for
`cpy_q_f32<helper,QK>`. These rows preserve the production finite layout/type
or helper/QK branch, inject the row-local positive extent guard (`total > 0` or
`num_blocks > 0`), keep `gridDim.x` symbolic and positive, and verify the
linearized ownership shape from the S481 production profile. S486-B covers
`L018`-`L021` and `L046`-`L056`, returns DRF with zero unknowns/errors for all
15 commands, and does not change manifest verdict fields, prove numeric
convolution/quantization equivalence, or parse the full host translation unit.

S486-C consumes the S481 im2col profile blockers into bounded symbolic
block-dimension launch-contract rows `L074` and `L075`. These rows do not
hardcode the earlier profile command's `blockDim.x = 256`. Instead they require
`--all-dims` and inject the production launch guard
`0 < blockDim.x <= local_extent`, `blockDim.x <= CUDA_IM2COL_BLOCK_SIZE`,
`CUDA_IM2COL_BLOCK_SIZE = 256`, `blockDim.y = blockDim.z = 1`, and positive
`gridDim.{x,y,z}`. This over-approximates the production expression
`MIN(local_extent, CUDA_IM2COL_BLOCK_SIZE)` while preserving the DRF ownership
property. Both `L074` and `L075` verify with zero unknowns/errors under the
bounded symbolic `blockDim.x` contract. S486-C still does not change manifest
verdict fields, prove numeric im2col equivalence, or parse the full host
translation unit.

S487 defines the exact-evidence manifest promotion policy without mutating the
manifest. The policy is owned by
`Launch_contract.exact_evidence_manifest_promotion_policy_carrier` and records
which evidence can become row-local coverage status: an exact
`--launch-contract` command, structured DRF JSON with zero unknowns/errors,
row-owned launch/profile/source facts, and an explicit soundness boundary. The
row-local status name is `drf_exact_row`. The existing solve-tri symbolic proof
remains a separate `guarded_symbolic_family_proof` status, because family-level
guarded proof is not the same thing as exact row-local launch-contract evidence.

The same S487 policy also records what must not promote: source-slice-only DRF
evidence, production-profile evidence without an executable launch-contract
command, row/family similarity, stale or missing artifacts, and numeric
equivalence claims that the DRF proof did not establish. Current policy
accounting records 53 exact row-local DRF rows, one guarded symbolic family
proof, zero source-slice/profile-only rows admitted to manifest promotion, and
preserves the remaining boundaries as 21 host-template/dependent-local blockers,
one indirect launch helper blocker, and eight unsupported-boundary families.
Manifest verdict fields remain unchanged; applying this policy to the manifest
is a separate step.

S488 is the next family-level frontier: unguarded symbolic family exploration.
The goal is to stop treating exact row-local proofs as the default proof unit
for new families. For each family, the verifier should attempt to generate the
broadest symbolic obligations it can while preserving semantic well-formedness
constraints such as positive CUDA dimensions, explicit subgroup configuration,
known source/launch relations, memory-space facts, alias/address-space facts,
and supported synchronization/subgroup semantics. The first solver result is
then classified as `unguarded_unsat`, `production_reachable_race`,
`invalid_counterexample_needs_guard`, `timeout`, `unsupported`, or
`extraction_blocked`.

S488 separates exploration from admission. An unguarded `unsat` result is
stronger than a production-guarded proof if the symbolic obligation really
over-approximates the production family. A `sat`/racy result is not promoted or
reported as a production bug until the counterexample is classified against the
launch/profile/source facts. Invalid shapes, missing source/launch relations, or
unsupported semantics become guard/extraction tasks, while production-reachable
counterexamples become race candidates. Coverage still flows through the S487
policy: source-slice/profile-only evidence and unclassified counterexamples do
not update manifest status.

S489 is the executable frontier runner for the S488 model over the full
ggml-cuda family inventory. `Launch_contract_generator` imports the 95-family,
146-row S474 frontier into target-owned OCaml data, preserving each family's
baseline blocked/unsupported stage and first blocker. `FAIAL_S489_ALL_FAMILY_FRONTIER_OUT`
emits `ocaml-s489-all-family-unguarded-frontier-v1`: one tabular row per
imported family, the first classification reached, the strongest artifact
status, the first blocker, and an explicit non-admission status. Current proof
evidence is an overlay on top of that baseline: S490 structural builders can
upgrade supported exact/profile-backed families to `unguarded_unsat`, and a
fresh solve-tri symbolic proof can upgrade F082 from imported blocker to the
solver's S488 classification. Imported blockers and unsupported boundaries
remain non-admitted.

S490 adds the first reusable broad-obligation builders to that frontier. These
builders are selected from row-owned exact launch/profile context, not from
family-name shortcuts. The current structural builders cover one-dimensional
elementwise ownership, linearized output ownership, block/lane ownership,
single-block copy ownership, single-thread scalar ownership, and bounded
symbolic `blockDim.x` ownership. With a fresh solve-tri report, the current
full-family frontier classifies 33 families as `unguarded_unsat`, keeps 54 as
`extraction_blocked`, and keeps eight as `unsupported`. Without the solve-tri
report, the count is 32 `unguarded_unsat`, 55 `extraction_blocked`, and eight
`unsupported`. The soundness boundary is intentionally narrow: S490 proves DRF
ownership for the recorded memory-shape class and exact launch/profile facts.
It does not prove numeric equivalence, full host translation-unit parsing,
unsupported atomics/CUB/inline assembly, or manifest promotion.

S491 is the blocker-retirement frontier over the current S489 results.
`FAIAL_S491_BLOCKER_RETIREMENT_OUT` emits
`ocaml-s491-blocker-retirement-frontier-v1`: one row per family that remains
`extraction_blocked` after the S488/S490 overlays, with the imported blocker
class, first blocker, priority, next action, and retirement status. With a
fresh solve-tri report, the current worklist contains 54 blocked families:
22 `launch_template`, 28 `preprocessing`, three
`row_derived_symbolic_family_guard`, and one `subgroup_config`. S491 selects
the 22 `launch_template` families as the first retirement target. It does not
promote coverage or change manifest status; a selected family still needs fresh
events, obligations, and solver or pre-solver classification before it can move
out of the blocker list.

S455 adds the next pure proof-boundary classifier for those validated
candidate facts. The classifier is owned by
`Symbolic_launch_evidence.guarded_candidate_boundary`: it consumes the
S453/S454 row facts plus the source-visible memory-effect, positive-shape, and
alias/address-space blocker statuses, rejects inferred or stale ready inputs,
and renders the exact lower blocker before `Memory_event` obligation
construction. For the current `L012` clamp seed, the boundary stops at
`not_ready_missing_checked_specialization_profile_fixture_and_positive_guard`
with zero obligations, no solver or pre-solver run, no lookup row, no guarded
family admission, and no manifest promotion. Completing this boundary for
`L012` still requires checked `T=half`/`T=float` specialization facts, a full
preprocessing/source profile or extraction fixture, positive `k`/zero-work
launch policy, and checked alias/address-space assumptions for `x` and `dst`.

S409 promotes only canonical `L118` in the launch manifest after the exact
production command exits successfully with structured subgroup JSON:

```bash
faial-drf --json --launch-contract L118 \
  --kernel solve_tri_f32_fast_l118 --block-dim '[32,16]' \
  --subgroup-size 32 -t 10000 \
  agent_results/rewrite/component_summaries/S408/artifacts/L118_solve_tri_f32_fast_l118_source_slice.cu
```

The accepted verdict is `status=drf`, `mem_drf=drf`,
`subgroup_uniformity=drf`, `drf_full=drf`, and two memory checks with zero
racy, unknown, timeout, or unsupported classifications. This does not promote
solve-tri neighbors `L116` or `L119`-`L128`, does not prove full-source
host-header intake for `solve_tri.cu`, and does not broaden shared-memory,
subgroup-helper, CUDA, or ggml-cuda support.

S427 makes the executable solve-tri support a first-class generated family
contract consumed by the production launch-contract lookup. The family record
is still bounded: it records source family `solve_tri_f32_fast`, source file
`llama.cpp/ggml/src/ggml-cuda/solve_tri.cu`, `n_template = 64`,
`blockDim.x = 32`, `blockDim.y = K`, `blockDim.z = 1`, dynamic shared memory
`0`, `warp_reduce_sum` as the reviewed subgroup helper, and explicit
`--subgroup-size 32`. Its admitted lookup rows are exactly `L117` and `L118`.
Rows `L119`-`L126` are recorded as unpromoted neighbors, and `L116`, `L127`,
and `L128` are recorded as excluded rows. Production lookup consumes only the
admitted rows; the family record is not a guarded family proof and does not
promote any neighbor without exact JSON or a future guarded proof artifact.

S430 adds a typed symbolic `K` guard record for the same solve-tri family
without admitting more lookup rows. The guard covers the manifest/source launch
cases `K = {32,16,14,12,10,8,6,4,2,1}` with fixed `N = 64`,
`blockDim = [32,K,1]`, dynamic shared memory `0`, subgroup helper
`warp_reduce_sum`, and explicit `--subgroup-size 32`. `L117` and `L118` remain
the only lookup anchors; `L119`-`L126` remain unpromoted guard candidates, and
`L116`/`L127`/`L128` remain excluded.

The current symbolic route is intentionally fail-closed before proof: the
launch-contract generator records a `Memory_event.Subgroup_obligation` blocker
because `Launch_contract.t` and `Dim3.t` still carry concrete template and
block-shape values, so the production subgroup obligation builder cannot yet
emit an obligation whose checked block dimension contains symbolic `K`. This
blocker is not a solver or pre-solver result, is not a guarded family proof,
and does not change manifest verdicts.

S433 adds a typed symbolic dimension carrier beside the exact solve-tri
lookup rows. The carrier preserves `n_template = 64`, `k_template = K`,
candidate values `K = {32,16,14,12,10,8,6,4,2,1}`, `blockDim = [32,K,1]`,
dynamic shared memory `0`, launch branch guards, positive shape guards,
unpromoted neighbor rows, excluded sentinel/helper rows, and explicit
`--subgroup-size 32`. Exact `L117` and `L118` lookup contracts still keep their
concrete `Dim3.t` block dimensions for regression runs, while the carrier is
available through the launch-contract boundary for the next
`Memory_event.Subgroup_obligation` step. Until that lower boundary consumes
the carrier and emits a fresh symbolic obligation containing `K`, S432 remains
blocked, no solver or pre-solver proof may run over symbolic `K`, and
`L119`-`L126` remain unpromoted.

S434 consumed that carrier at the subgroup-obligation boundary and emitted a
Faial-owned symbolic-dimension blocker record. The record contains the route
owner, source family, symbolic `K` candidate values, `blockDim = [32,K,1]`,
lookup anchors, unpromoted rows, excluded rows, and `obligation_count = 0`.
It names the first concrete-only lower boundary:
`checked_invocation_domain_condition` still consumed `Dim3.t`, so real
subgroup obligation goals could not yet encode `blockDim.y = K`. This is a
fresh handoff artifact and exact blocker, not solver input, not a guarded
family proof, and not permission to promote `L119`-`L126`.

S437 extends `Memory_event.Subgroup_obligation` with typed checked block
dimensions that can be concrete or symbolic. Existing callers may still pass a
concrete `Dim3.t`; that path keeps the current facts `blockDim.{x,y,z} =
<concrete>` and bounds projected tasks against `blockDim.{x,y,z}`. The
generic `checked_dimension` / `checked_block_dim` API lives in
`Memory_event.Subgroup_obligation`; solve-tri carrier interpretation lives
outside the proof primitive in `Symbolic_launch_evidence`. That module converts
the solve-tri symbolic carrier into a checked block dimension `[32,K,1]` whose
obligation-domain facts include `blockDim.y = K`, `K > 0`,
`0 <= threadIdx.y$T1 < K`, and `0 <= threadIdx.y$T2 < K`. The conversion
remains guarded by the existing row-derived facts: fixed `N = 64`, candidate
`K` values `{32,16,14,12,10,8,6,4,2,1}`, dynamic shared memory `0`, route
owner `Memory_event.Subgroup_obligation`, and explicit subgroup size `32`.
Missing or non-positive symbolic candidates, missing positive guards,
unexpected route owners, or missing subgroup size fail explicitly. S437 does
not run solver/pre-solver proof, does not promote `L119`-`L126`, and keeps
`L116`/`L127`/`L128` excluded.

S438 connects the same checked-dimension carrier to the production
`drf/bin/app.ml` subgroup launch-contract route for evidence emission, but the
artifact rendering and solve-tri row ledger are owned by
`Symbolic_launch_evidence`, not by `Memory_event` or the CLI driver. When
`FAIAL_S438_SYMBOLIC_OBLIGATION_OUT` is set on a validated solve-tri
`--launch-contract` run, the route writes a fresh Faial-owned artifact
containing the route owner, launch row, family guard, executable source/launch
facts, checked-domain facts, candidate/anchor/unpromoted/excluded row sets, and
the generated symbolic `Subgroup_obligation` goals. For the guarded
`solve_tri_f32_fast<N,K>` route, the generated carrier records the source-width
relation `k == K` and the finite candidate domain
`K in {32,16,14,12,10,8,6,4,2,1}`. `Symbolic_launch_evidence` rewrites exact
anchor-row facts such as `k == 32` into that source-width relation and appends
the finite `K` domain before building symbolic obligations. If the carrier does
not name the relation, if the relation mismatches the symbolic block dimension,
or if no exact source-width fact is found in the source slice, the route fails
closed. The artifact is generated from the parsed subgroup kernel and its
ordinary source memory effects; it is not synthesized from task-local JSON or
historical S404/S409/S427-S429 artifacts. Normal DRF verdicts still use the
concrete row-local block dimensions for `L117` and `L118`, and the S438 artifact
records `symbolic_solver_run: false`. Symbolic solver or pre-solver consumption
remains a separate S439 boundary, and manifest promotion remains separate from
artifact generation.

S439 consumes those same fresh production-route symbolic obligations through
an internal evidence hook, `FAIAL_S439_SYMBOLIC_PROOF_OUT`. The hook
regenerates the symbolic checked-domain obligations through
`Symbolic_launch_evidence`, then sends those symbolic goals to
`Subgroup_solver` without passing the concrete row-local `Dim3.t` fallback.
This records the actual solver classification over `blockDim.y = K`, source
relation `k == K`, and the finite guarded `K` domain rather than reusing the
exact `L117`/`L118` concrete proof route. For the current solve-tri source-slice
artifact, S439 classifies the two generated symbolic memory obligations as
`unsat(drf)` and records `guarded_family_proof: true` over rows `L117`-`L126`.
S439 still adds no pre-solver rule and performs no manifest promotion by
itself; it is an evidence hook that reports whether the generated symbolic
family obligations are proved, racy, unknown, timed out, or unsupported.

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
- The subgroup source-effect route does not currently run the ordinary MAP
  `Aligned -> Delin` pipeline. Consequently `--assume-delin` still controls
  ordinary kernels but does not add inferred multidimensional index bounds to
  subgroup obligations. User `--assume` clauses are likewise applied in
  `prepare_pre` after ordinary protocol lowering and are not subgroup launch
  preconditions yet. An upstream ordinary-path DRF result that depends on
  either class of assumption is not yet a subgroup-path parity result; the
  subgroup alarm remains conservative until the same facts are carried with
  explicit provenance. Launch extraction has the same boundary: a
  specialization chosen by a host `switch` needs that case predicate in
  `LaunchParam`; template identity alone does not justify inventing a runtime
  equality.
- If subgroup phase reasoning needs subgroup identity, the caller must provide
  an explicit target configuration and the checked block dimensions. Missing
  configuration or missing block dimensions are unsupported boundaries, not
  assumed warp size or unbounded-thread fallbacks.
- Subgroup-aware memory obligations constrain both projected tasks to the
  checked invocation domain:
  `0 <= threadIdx.{x,y,z}$Tn < checked_bound.{x,y,z}`, plus concrete or
  symbolic `blockDim.{x,y,z}` facts for the checked kernel configuration. For
  concrete `Dim3.t` inputs, checked bounds remain `blockDim.{x,y,z}`. For the
  accepted solve-tri symbolic carrier, the y bound is symbolic `K`, and the
  goal also records `blockDim.y = K` and `K > 0`. When checked block
  dimensions are supplied, these facts are included for every generated
  obligation, including ordinary same-phase obligations that do not otherwise
  need subgroup identity. Every obligation also includes the CUDA grid domain
  `0 <= blockIdx.{x,y,z} < gridDim.{x,y,z}`. Counterexamples that require a
  block index at or beyond the launched grid are therefore rejected before
  they can be reported as races.
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
  them to `Subgroup_solver`. Without this option, parsing, MAP construction,
  symbolic obligations, and Z3 dispatch use the upstream Faial path unchanged.
  Therefore a no-flag result for source containing subgroup/matrix operations
  is only an upstream-compatibility result, not a subgroup-aware verdict.
- A synthesized launch wrapper is linked before routing. The subgroup carrier
  receives the callee body, launch assertions, actual argument bindings, and
  exact integral template specialization bindings. Declaration-valued
  non-type arguments are compile-time identity only and are admitted only when
  Clang has already removed the corresponding parameter from the specialized
  body. Exact global cooperative-launch targets and auxiliary specializations
  share the same lookup. Missing definitions, ambiguous overloads, mismatched
  template metadata, or unresolved specialization parameters fail with
  wrapper/callee diagnostics. Isolated wrapper analysis remains forbidden
  because it loses the production memory behavior.
- Cached CUDA JSON inputs (`*.cjson`) are accepted by the same public analyzer
  route as live CUDA source. When `--assume-launch` is present, cached
  `LaunchParam` entries are rewritten by the launch-synthesis pass before the
  MAP pipeline runs. This keeps cached-AST comparisons aligned with live
  `cu-to-json --launch-params` runs without treating a stale or launch-free
  JSON artifact as proof evidence.
- `--subgroup-size` validates `N` as a positive subgroup size and records the
  target as `cuda-like(threadIdx.x-contiguous(size=N))`. The model still does
  not infer warp size or lane mapping from the source.
- Subgroup memory ordering that needs subgroup identity also needs checked
  block dimensions. They may come from an explicit `--block-dim` or a complete
  three-axis synthesized launch assertion. Missing, partial, conflicting, or
  non-positive checked dimensions remain unsupported or vacuous boundaries,
  not fallback assumptions.
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
- The ordinary ownership pre-solver is also opt-in. `Solve_drf` invokes it only
  when `--launch-contract` supplies the guarded source/launch facts; standard
  Faial commands send canonical `Symbexp` obligations directly to Z3.
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
