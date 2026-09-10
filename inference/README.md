# Inference

Inference translates source ASTs into Faial's memory-access representation.
C/CUDA and WGSL ASTs are read from JSON:

```text
CUDA JSON -> C_lang -> D_lang -> Imp -> memory-access protocol
WGSL JSON -> W_lang -> Imp -> memory-access protocol
```

`D_lang` makes expression-embedded memory accesses explicit statements.
`D_to_imp` and `W_to_imp` perform the ordinary lowering. The subgroup
extension branches from normalized CUDA `D_lang`, before ordinary lowering
loses warp-operation details.

## CUDA entry points and helpers

`__global__` entry points and `__device__` helpers keep their visibility
through the pipeline. `C_lang` and `D_lang` use `KernelAttr.Default` and
`KernelAttr.Auxiliary`; `Imp` uses `Visibility.Global` and
`Visibility.Device`. Helpers remain available for inlining but are not
independent entry points. The `c-ast --only-global` option filters entry points.

`Subgroup_source.route_program` classifies kernels. With explicit subgroup
configuration, supported warp operations or WMMA calls select the subgroup
route, including operations reached through device helpers. Ordinary kernels
retain their `D_lang.Kernel` identity and are resolved against
`Protocol_parser.d_program_to_proto` for the complete program. Do not lower an
ordinary entry without its helper definitions.

Helper resolution uses arity, function type, and template specialization
metadata. It supports selected member-helper chains such as
`block_reduce_policy::reduce`. Unsupported forms are reported explicitly;
they must not become empty helper bodies.

Kernel names receive source-order suffixes before selection. `--kernel`
selects an entry without dropping its helper context. Unsupported unselected
entries do not reject the selected route.

Without `--subgroup-size`, the CLI deliberately uses upstream lowering.
That compatibility route does not prove subgroup participation. Parser-only
CUDA shims in `cuda_include/` are added by subgroup-aware entry points, not by
ordinary `Cu_to_json` calls.

## Launch wrappers

`--assume-launch` wrappers are linked to their terminal kernel call before
routing. Linking preserves grid/block/path assertions, binds arguments and
integral template parameters, and alpha-renames callee locals.

Template metadata distinguishes specializations with the same name or
signature. Declaration-valued arguments are accepted only when the specialized
body no longer contains their unresolved template parameter. Missing,
ambiguous, or unresolved targets are errors, not empty kernels.

Exact block-dimension equalities are stored separately from general launch
preconditions. The DRF layer validates the dimension tuple and checks
precondition satisfiability when requested.

## Subgroup representation

`Subgroup_matrix` holds target configuration and statements for block
barriers, warp barriers, warp collectives, and matrix operations.
`Subgroup_source.subgroup_kernel` adds ordinary memory effects and source
facts:

- Static site identifiers, source order, labels, and locations.
- Enclosing branch, loop, switch, and exit conditions for participation.
- Numeric aliases and source-uniform variables at each operation site.
- Ordinary reads, writes, and atomics with array indices, guards, and phases.
- Memory-global variables, launch preconditions, and launch dimensions.

The configured CUDA mapping is x-contiguous and requires an explicit subgroup
width. Do not infer that width from a source variable named `WARP_SIZE`.

Recognized collectives include `warp_sum`, `warp_max`,
`warp_reduce_sum`, `warp_reduce_max`, `warp_reduce_all`,
`warp_reduce_any`, and the supported `__shfl*_sync` forms.
Direct masked calls and `__syncwarp` require a statically full mask.
Recognized reduction/vote helpers return a subgroup-uniform value when their
control is subgroup-uniform. A shuffle's result is not generally uniform.

Loop backedges, exits, and repeated helper calls preserve control metadata and
mark sites that may repeat. The memory checker decides how to handle repeated
barriers and align their iterations.

The collector currently advances ordinary-memory phase tags at collectives as
well as barriers. The DRF event builder does not give collective boundaries
that ordering. See [the DRF guide](../drf/README.md#current-boundaries-to-review)
for this known representation mismatch.

## Scalar and pointer facts

Kernel parameters and expressions derived from uniform inputs can establish
participation uniformity. Memory-global facts are narrower: a value shared
within one warp must still be projected separately for two threads in different
warps.

Aliases carry dependency information. Reassignment invalidates dependent facts,
and control-flow joins keep only facts preserved on every outgoing path.
Memory-free helper summaries may preserve scalar facts when all applicable
overloads satisfy the summary. Unsupported memory-reading calls do not create
uniformity facts.

`Protocols.Exp` supplies the shared nonzero-divisor conditions used by source
extraction and participation proofs. Conditions retain their source order for
stable diagnostics.

Pointer aliases with a known base and offset are resolved before recording
accesses. Array declarations remain storage declarations, including Clang's
empty-constructor representation of implicit initialization. They must not be
reinterpreted as aliases. Unresolved aliases required by subgroup extraction
are errors; these strict checks do not alter ordinary `D_to_imp` lowering.
Recognizing a `__restrict__` pointer type does not itself add a no-alias proof.

## WMMA

The supported source forms are:

```text
fill_fragment(fragment, value)
load_matrix_sync(fragment, pointer, leading_dimension)
mma_sync(d, a, b, c)
store_matrix_sync(pointer, fragment, leading_dimension, layout)
```

The representation records fragment shape and matrix load/store footprints.
Pointer aliases and leading dimensions remain symbolic where supported.
Unsupported forms fail explicitly. The `mma.h` shim supplies declarations
for parsing, not memory semantics. Current DRF obligations cover ordinary
source memory effects; retaining a matrix footprint does not mean that the
checker verifies its memory accesses.

## Tests

From the repository root:

```bash
opam exec --switch=. -- dune runtest inference/test
```

The AST-level tests cover routing, launch linking, helper and template
resolution, pointer/scalar facts, source conditions, repeated sites, and matrix
representation without requiring a CUDA translator. End-to-end CUDA examples
also require a working `cu-to-json`; see the [DRF guide](../drf/README.md#build-and-test).
