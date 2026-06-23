# Inference

The *inference* stage (module `infer`) is a translation between a source language AST (eg, C/CUDA, WGSL) and the intermediate `Imp` representation used by Faial.

Our inference stage supports 2 ASTs: C/CUDA and WGSL. In both languages the AST is read from a JSON file, then inference takes the AST and generates an equivalent Imp, depicted as follows:

```
JSON → Source Language AST → Imp → Memory Access Protocol
```

For C/CUDA, this involves a two-stage transformation:
```
JSON (c-to-json) → C_lang → D_lang → Imp → Memory Access Protocol
```

For WGSL, this involves a single-stage transformation:
```
JSON (WGSL AST) → W_lang → Imp → Memory Access Protocol
```

**Module structure:**

- **`c_lang.ml`** - C/CUDA AST definitions and JSON parsing from c-to-json; mirrors LLVM's AST
- **`d_lang.ml`** - C/CUDA AST where memory-accesses are made into statements (not expressions); extracted from C_lang
- **`d_to_imp.ml`** - C/CUDA to Imp translation (via D_lang intermediate)
- **`w_lang.ml`** - WGSL AST definitions
- **`w_to_imp.ml`** - WGSL to Imp translation

## Kernel Attributes: `__global__` vs `__device__`

CUDA kernels have two visibility modifiers that are preserved throughout the translation pipeline:

- **`__global__`**: Entry point kernels callable from host code
- **`__device__`**: Auxiliary functions callable only from GPU code

**Representation across pipeline stages:**

| Stage | Field | Type | `__global__` | `__device__` | Helper Function |
|-------|-------|------|--------------|--------------|-----------------|
| **C_lang** | `kernel.attribute` | `KernelAttr.t` | `Default` | `Auxiliary` | `KernelAttr.is_global` |
| **D_lang** | `kernel.attribute` | `KernelAttr.t` | `Default` | `Auxiliary` | `Kernel.is_global` |
| **Imp** | `kernel.visibility` | `Visibility.t` | `Global` | `Device` | `Kernel.is_global` |

Each stage provides an `is_global` helper function for consistent filtering and analysis. The `c-ast` tool supports `--only-global` flag to display only `__global__` kernels across all three stages.


## C/CUDA Pipeline: `c_lang → d_lang → d_to_imp → Imp`

**Modules**: `c_lang.ml`, `d_lang.ml`, `d_to_imp.ml`

**Key characteristics**:
- **Two-stage translation** required due to expression-embedded memory operations
- **JSON input** from c-to-json tool (LLVM AST dump)
- **Memory extraction** via state monad in `d_lang.ml`
- **CUDA include shims** in `inference/cuda_include/` are parser-only
  declarations added to the local `cu-to-json` invocation by
  `Cu_to_json.cu_to_json_res`. The WMMA shim (`mma.h`) lets Clang capture
  focused `nvcuda::wmma` declarations and calls, but it does not add DRF
  semantics.

### CUDA WMMA Source Surface

The inference boundary recognizes the focused CUDA WMMA call names
`fill_fragment`, `load_matrix_sync`, `mma_sync`, and `store_matrix_sync`.
These calls route through the first-class `Subgroup_matrix` carrier when an
explicit subgroup target configuration is supplied. They must not be translated
as ordinary calls, scalar memory accesses, or workgroup
synchronization points, because that would allow the workgroup DRF pipeline to
report a false workgroup-only verdict for matrix collective code.

This is source-surface support only. Unsupported WMMA forms should fail
explicitly rather than falling back to guessed workgroup-only semantics.

### CUDA Dependency Preservation

The `D_lang` to `Imp` boundary preserves focused scalar, loop, helper, and
constant dependencies that feed later subgroup/matrix analysis:

- scalar helper calls such as `min`, `max`, `fminf`, `fmaxf`, `divUp`, and the
  existing Faial predicate helpers are lowered to expression dependencies;
- focused conversion and warp helper calls such as `__half2float`,
  `__float2half_rn`, `warp_sum`, `warp_max`, `warp_reduce_sum`,
  `warp_reduce_max`, `__shfl_sync`, and `__shfl_down_sync` are kept as
  explicit dependency summaries instead of being silently dropped;
- global `const int` declarations and loop bounds/steps remain visible in the
  generated `Imp` preamble and loop ranges.

Subgroup-only operations also route through the subgroup/matrix carrier when
the required target configuration is present. For example, `__syncwarp` is not
treated as a workgroup barrier or a no-op in ordinary `Imp` lowering.

### CUDA Pointer Alias Boundary

The C/CUDA source boundary preserves focused pointer identity forms that later
subgroup/matrix work needs:

- pointer alias declarations and assignments with a concrete base plus offset,
  such as `tile_base = tile + base` and `z = y + i`, lower to `LocationAlias`
  entries before later memory accesses are scoped;
- `__restrict__`-qualified pointer parameters are still recognized as pointer
  array parameters by the ordinary `Imp` intake, but the current workgroup DRF
  path does not attach a no-alias semantic guarantee to that qualifier;
- WMMA calls are still rejected before ordinary `Imp` lowering, and the
  unsupported boundary keeps the call text, including matrix pointer arguments
  such as `tile_base + d`, available in diagnostics.

Conditional pointer aliases require source-parameter-aware alias resolution.
Until the first-class subgroup/matrix representation owns that path, unresolved
forms such as `KV_OVERLAP ? K : V` fail with `Unsupported_source` rather than
falling back to a guessed base pointer.

### Subgroup/Matrix Representation Boundary

The `Subgroup_matrix` module is the first-class OCaml carrier for the focused
subgroup and WMMA extension. It records:

- stable source-site ids, labels, and optional source locations;
- explicit CUDA subgroup configuration, currently the x-contiguous mapping
  where `threadIdx.x / subgroup_size` is the subgroup id;
- workgroup barriers separately from subgroup barriers and subgroup
  collectives;
- matrix collectives for `fill_fragment`, `load_matrix_sync`, `mma_sync`, and
  `store_matrix_sync`;
- matrix memory effects attached to the matrix collective site, with
  `load_matrix_sync` requiring a read footprint and `store_matrix_sync`
  requiring a write footprint;
- rectangular matrix footprints that preserve base pointer, row count, column
  count, leading dimension, layout, row witness, and column witness.

This module does not itself dispatch CUDA source onto the subgroup path; that
is owned by `Subgroup_source`, while DRF obligations and solver taxonomy remain
owned by `drf`. Missing subgroup configuration remains an explicit error when
subgroup identity is queried, and matrix footprints are not collapsed to a
scalar `ptr[0]` fallback.

### Source-To-Subgroup Dispatch Boundary

The `Subgroup_source` module is the scoped bridge from normalized CUDA
`D_lang` kernels into either the ordinary `Imp` path or the `Subgroup_matrix`
carrier:

- kernels without WMMA or subgroup operations still route to ordinary `Imp` and
  do not require subgroup configuration;
- kernels containing `__syncwarp`, focused CUDA warp helper/shuffle calls, or
  focused WMMA calls route to `Subgroup_matrix` and require an explicit target
  configuration, currently CUDA x-contiguous
  `threadIdx.x / subgroup_size`;
- the supported focused WMMA call shapes are exact:
  `fill_fragment(fragment, value)`, `load_matrix_sync(fragment, pointer, ldm)`,
  `mma_sync(d, a, b, c)`, and
  `store_matrix_sync(pointer, fragment, ldm, layout)`;
- `load_matrix_sync` and `store_matrix_sync` build rectangular matrix
  footprints from the fragment type, pointer expression, leading dimension, and
  explicit layout from the fragment type or store call;
- focused pointer alias declarations and assignments feed matrix pointer
  footprints as base-plus-offset aliases instead of collapsing to the alias
  variable;
- fragment row/column dimensions may remain symbolic when the source uses
  constants such as `WMMA_M` and `WMMA_N`;
- `fill_fragment` and `mma_sync` become matrix collective sites without direct
  source-visible memory effects;
- focused CUDA warp helper/shuffle calls, currently `warp_sum`, `warp_max`,
  `warp_reduce_sum`, `warp_reduce_max`, `__shfl_sync`, and
  `__shfl_down_sync`, become subgroup collective sites without direct
  source-visible memory effects. They advance only the subgroup phase, so
  ordinary source memory effects collected before and after those calls remain
  in the same workgroup phase but can be ordered later only for same-subgroup
  invocations. The `warp_reduce_*` names are helper summaries used by
  ggml-cuda-style source slices; they do not infer subgroup size from
  `WARP_SIZE`. Direct `__shfl_xor_sync` width-sensitive modeling remains an
  unsupported boundary until a guarded width/subgroup-size rule is added;
- subgroup and WMMA sites collected from source control constructs carry the
  enclosing branch, loop, switch, case, or default condition as adjacent
  site-control metadata for the DRF uniformity checker; unsupported control
  expressions fail explicitly instead of being treated as top-level control.
  Matrix load/store sites also carry a separate memory-control list for DRF
  obligations. The uniformity control remains the source participation guard;
  the memory control additionally includes scalar alias equalities needed by
  the rectangular matrix footprint, such as base offsets, subgroup-id aliases,
  and loop induction facts;
- kernel parameters, member fields rooted in kernel parameters, and local or
  loop variables initialized from syntactically uniform expressions are passed
  as source-uniform variables; locals derived from subgroup-varying expressions
  remain non-uniform;
- the source bridge also tracks a narrower `memory_globals` set for arithmetic
  facts that must remain shared across projected DRF tasks. Kernel parameters,
  CUDA block/grid dimensions, and locals derived only from those memory-global
  inputs may be projected without a task suffix downstream. Subgroup-owned
  source-uniform locals such as subgroup row aliases or configured subgroup-id
  aliases are not memory globals, so row and lane ownership remains
  task-local in memory obligations. Branch and loop exits use the same
  fail-closed join discipline as other scalar facts: a memory-global fact
  survives only when every reachable outgoing path preserves it;
- CUDA thread-coordinate member expressions are canonicalized before source
  uniformity metadata is inferred. For an explicit x-contiguous subgroup
  configuration, a local alias such as `tid = threadIdx.x` is tracked only as a
  thread-x coordinate alias, not as a uniform value; `warp_id = tid /
  subgroup_size` is accepted as a source-uniform subgroup-id alias only when
  the divisor matches the configured subgroup size. Later scalar assignments
  replace the tracked facts for the assigned local: reassignment from
  `tid % subgroup_size`, lane aliases, calls, unknown locals, or assignments
  reached under subgroup-varying source control clear the source-uniform fact
  and leave later subgroup participation to fail closed. Lane aliases such as
  `tid % subgroup_size` remain subgroup-varying. Branch and loop exits merge
  scalar facts conservatively for future statements: a uniform,
  thread-coordinate, or constant fact survives only if every reachable outgoing
  path carries the same fact/value, even when the fact was introduced inside
  the construct. Facts visible while collecting a subgroup or WMMA site are
  snapshotted with that site's control metadata. Sibling `if` branches collect
  those snapshots from the scalar facts valid at the branch entry, so a fact
  introduced by the `then` branch is not visible to subgroup or WMMA sites
  emitted by the `else` branch. This keeps source-local loop controls
  checkable without making one-path facts globally true after the construct;
- ordinary source memory effects collected from a kernel that routes to the
  subgroup/matrix path are recorded beside the matrix carrier as structured
  source effects. Each record preserves the access mode, base/index access,
  source site label/location, source-order ordinal, enclosing source-control
  conditions, current workgroup phase, current subgroup phase,
  runtime-condition placeholder, and explicit target configuration. Source
  conditions include relevant scalar
  alias equalities reachable from the access and its guards, including local
  aliases assigned under lane-varying control. Alias and guard expressions also
  contribute definedness facts for division and modulo denominators, such as
  `wg_per_batch != 0` for `blockIdx.x / wg_per_batch`, because those source
  expressions must be defined on any path that reaches the memory access.
  Duplicate source conditions are removed before they are attached to the
  effect. Positive-stride `for` loops add ownership facts of the form
  `(i - init) % step == 0` and `init <= i` for the loop body without leaking
  `i == init` as an invariant. Effects are not silently dropped by the
  analyzer: the `drf` subgroup-memory boundary consumes these rows as
  subgroup-aware memory obligations. Numeric aliases are
  assignment-sensitive across dependencies: when a scalar is overwritten,
  aliases whose right-hand side depends on that scalar, directly or
  transitively, are removed before later ordinary-memory or matrix
  memory-control conditions are emitted. This prevents stale chains such as
  `idx == lane && lane == 0` from describing an `idx` value captured before
  `lane` was reassigned. Pointer aliases whose stored offsets depend on those
  invalidated scalar aliases are also removed and later uses of the stale
  pointer local fail explicitly instead of being reinterpreted as either the
  old address or an unrelated base pointer. Local pointer declarations without
  an initializer are invalid until a supported pointer expression assigns
  them. Branch and loop exits join pointer aliases with the same fail-closed
  CFG discipline as scalar facts: a pointer alias survives only when every
  reachable outgoing path carries the same resolved base and offset; otherwise
  the local is marked invalid so later matrix pointer use fails explicitly.
  Sibling branches are collected from the pointer facts valid at branch entry,
  so an alias created or invalidated in one branch cannot feed a matrix site in
  the other branch;
- unsupported matrix argument shapes, including missing or extra operands,
  missing fragment shape/layout, and unresolved pointer forms fail explicitly.

This bridge exports a deterministic site summary for tests and later artifact
comparison work. Site controls and ordinary memory effects carry source-order
ordinals so downstream DRF code can compare source-derived event order without
reconstructing it from source locations or display text. The `faial-drf`
subgroup path consumes site-control metadata for uniformity checking, while
DRF phase semantics and solver obligations remain owned by the `drf` library.

### Subgroup/Matrix Artifact Export

`Subgroup_source` now exposes deterministic artifact summaries for the
subgroup/matrix carrier. The site summary records the kernel target
configuration and each workgroup, subgroup, or matrix site in source order. The
matrix footprint summary records only matrix load/store memory effects and
keeps the rectangular base footprint, indexed access, and row/column bounds
witnesses.

For this artifact path, `J_type` exposes Clang `desugaredQualType` when
present. That lets local aliases such as `acc_frag_t`, `q_frag_t`, and
`v_frag_t` retain their underlying `nvcuda::wmma::fragment<...>` shape without
guessing a fallback type.

The manual test executable `inference/test/emit_subgroup_artifact.exe` emits
the current focused OCaml artifact for comparison against the Rust oracle, for
example:

```sh
opam exec -- dune exec inference/test/emit_subgroup_artifact.exe -- \
  --cu-to-json=./bin/cu-to-json \
  --kernel flash_attn_wmma_mirror_kernel \
  --subgroup-size 32 \
  --ignore-asserts \
  -D FAIAL_CAPTURE \
  -D '__float2half_rn(x)=((half)(x))' \
  ../flash_attn_wgsl_matrix_mirror.cu
```

This artifact is not a DRF verdict and is not a user-facing CLI contract. It is
an internal V501+ comparison boundary: unsupported source or representation
gaps must still fail explicitly, and non-WMMA CUDA stays on the ordinary `Imp`
route without requiring subgroup configuration. The artifact includes
`uniform_vars`, `site_controls`, and `ordinary_memory_effects` sections for
subgroup-routed kernels. `site_controls` may print
`memory_control=` when the matrix memory guard is richer than the uniformity
participation guard. The uniform/control rows explain what the `drf`
uniformity checker consumes, while matrix memory-control and ordinary-memory
rows are source-carrier evidence that the `drf` stage consumes when building
subgroup memory obligations.

The C/CUDA translation pipeline uses a unique two-stage approach that separates parsing concerns from analysis preparation:

#### Stage 1: C_lang (JSON Parser and Raw AST)
**Purpose**: Handle the complexity of parsing LLVM/Clang JSON output
- **Input**: JSON from c-to-json tool (LLVM AST dump)
- **Output**: Raw C/CUDA AST that faithfully mirrors LLVM's structure
- **Responsibilities**:
  - JSON parsing with comprehensive error handling
  - Direct mapping to LLVM AST node types (100+ expression variants)
  - Graceful handling of invalid/recovery expressions
  - Preservation of all source language constructs
  - Type system integration (`J_type.t` from JSON)

#### Stage 2: D_lang (Analysis-Ready Transformation)
**Purpose**: Extract memory operations from expressions into statements (required by Imp)
- **Input**: `C_lang.Program.t`
- **Output**: AST where all memory accesses are explicit statements
- **Core Challenge**: C allows memory operations within expressions, but Imp requires memory accesses to be statements
- **Transformations**:
  - **Memory Access Extraction**: Convert array subscripts and pointer dereferences from expressions to explicit read/write statements
  - **Expression Decomposition**: Split complex expressions containing memory operations into statement sequences
  - **Side Effect Isolation**: Use state monad to track and extract memory accesses during expression evaluation
  - **Statement Generation**: Generate explicit `ReadAccessStmt` and `WriteAccessStmt` for analysis

**Example transformation**:
```c
// C source: memory access within expression
int result = array[i] + array[j];

// C_lang: represents as nested expressions
BinaryOperator {
  opcode = "+";
  lhs = ArraySubscriptExpr {lhs=array; rhs=i};
  rhs = ArraySubscriptExpr {lhs=array; rhs=j}
}

// D_lang: extracts memory accesses as statements
ReadAccessStmt {target=temp1; source=array[i]; ty=int}
ReadAccessStmt {target=temp2; source=array[j]; ty=int}
AssignStmt {var=result; data=temp1 + temp2}
```

#### Why Two-Stage Architecture is Required

1. **What Imp Needs**:
   - All memory accesses must be explicit statements for analysis
   - Memory access patterns must be extractable for protocol generation
   - Temporal ordering of reads/writes must be preserved

2. **What C Provides**:
   - Memory operations embedded within expressions: `a[i] + b[j]`
   - Nested array subscripts: `arr[i][j][k]`
   - Pointer arithmetic mixed with computation: `*(ptr + offset) * 2`

3. **How We Bridge The Gap**:
   - **Two-stage translation**: C_lang → D_lang → Imp
   - **State monad transformation**: Extract memory operations from nested expressions
   - **Temporary variables**: Store results of extracted memory statements
   - **D_lang intermediate**: Isolates the complexity of memory operation extraction

## WGSL Pipeline: `w_lang → w_to_imp → Imp`

**Modules**: `w_lang.ml`, `w_to_imp.ml`

**Key characteristics**:
- **Single-stage translation** sufficient due to structured memory model
- **Direct AST input** (no complex JSON parsing needed)
- **Address space annotations** explicitly map to memory hierarchy
- **Structured types** enable straightforward field access flattening

## Future work

### Parsing C/CUDA with FrontC

We considered implementing a C/CUDA parser based on [FrontC](https://github.com/BinaryAnalysisPlatform/FrontC/).

Motivation:

- *simplifies building Faial:* would make parsing C/CUDA self-contained (no external dependencies)
- *gives a fallback alternative to c-to-json,* which is infamously difficult to build
- *faster parsing times:* obviates the need for spawning a process for parsing, and marshaling JSON
- *enables Faial running in browser:* allows JS and have it run browser-side
- *significant overlap with `c_lang.ml`*, as both LLVM and FrontC are parsing C, we would expect a significant overlap of both ASTs

Current limitations:
- *AST has no typing information,* which means that we would need to type-check it, introducing a stage before `c_lang.ml`; incorrect typing information would need to conservatively assume that types are integers (thus reducing the efficacy of our code slicing) and introduce false alarms (as larger ranges for integers would be incorrectly assumed)
- *AST has no provenance information,* so we would lose the ability to pin-point errors, thus breaking the UI
