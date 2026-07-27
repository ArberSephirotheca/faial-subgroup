open Stage0

(* -------- Define the actual tests: ------------- *)

let tests =
  [
    (* The example should be DRF *)
    ("parse-gv.cu", [], 0);
    (* Unless we override the parameters with something other than
     what is in the source code. *)
    ("parse-gv.cu", [ "--gridDim=3" ], 1);
    (* This is the simplest data-race. *)
    ("racy-saxpy.cu", [], 1);
    (* Sanity check, make sure that the bit-vector logic works. *)
    ("racy-saxpy.cu", [ "--logic"; "QF_AUFBV" ], 1);
    (* This is the simplest data-race free example. *)
    ("drf-saxpy.cu", [], 0);
    (* The kernel contains constraints that makes it DRF: blockDim.{y,z}=1
     and gridDim.{y,z}=1. *)
    ("drf-saxpy.cu", [ "--all-dims"; "--all-levels" ], 0);
    (* Same kernel as drf-saxpy.cu but without the in-source __assume()s:
     racy under all-dims/all-levels because blockDim.{y,z} or gridDim.{y,z}
     may exceed 1, allowing two threads to compute the same index. *)
    ("drf-assume.cu", [ "--all-dims"; "--all-levels" ], 1);
    (* Same kernel, but the missing __assume() constraints are injected via
     the --assume CLI flag. Two assumptions are passed to verify that
     --assume composes when repeated. *)
    ( "drf-assume.cu",
      [
        "--all-dims";
        "--all-levels";
        "--assume";
        "blockDim.y == 1 && blockDim.z == 1";
        "--assume";
        "gridDim.y == 1 && gridDim.z == 1";
      ],
      0 );
    (* An in-source __assume() inside a loop mentioning the loop counter is
     routed onto the loop as an invariant; [i == threadIdx.x] discharges the
     write's cross-thread collision. *)
    ("drf-loop-assume.cu", [], 0);
    (* The same kernel without the __assume is racy. *)
    ("racy-loop-assume.cu", [], 1);
    (* The same constraint injected through the --assume CLI [binder=]
     target instead of an in-source __assume: it must resolve to the loop
     counter [i] and route onto the loop, clearing the race. *)
    ("racy-loop-assume.cu", [ "--assume"; "binder=i: i == threadIdx.x" ], 0);
    (* Same idea across a __syncthreads(): the loop is aligned (its first
     iteration peeled) and the invariant follows the peeling into every
     phase, so the post-barrier write stays race-free. *)
    ("drf-loop-sync-assume.cu", [], 0);
    ("racy-loop-sync-assume.cu", [], 1);
    (* __builtin_assume(cond), clang's assumption builtin, is honoured as a
     precondition like the __assume() stub: [tid < D] forces [tid % D == tid]
     so each thread writes a distinct cell and the kernel is DRF. *)
    ("drf-builtin-assume.cu", [], 0);
    (* Same kernel without the __builtin_assume: with D free the prover picks
     D = 1 and every thread aliases onto out[0], so it is racy. *)
    ("racy-builtin-assume.cu", [], 1);
    (* This example is only racy at the grid-level *)
    ("racy-grid-level.cu", [], 0);
    ("racy-grid-level.cu", [ "--grid-level" ], 1);
    (* This is a data-race in a 2D shared array. *)
    ("racy-2d.cu", [], 1);
    (* This is a data-race on a shared scalar. *)
    ("racy-shared-scalar.cu", [], 1);
    (* A data-race in shared memory is invisible at the grid level. *)
    ("racy-shared-scalar.cu", [ "--grid-level" ], 0);
    (* A data-race free example that relies on top-level assignments. *)
    ("drf-toplevel.cu", [], 0);
    (* A data-race that occurs when analysis understand top-level assignments.
     We ensure it's a data-race between threads 0 and 1. *)
    ("racy-toplevel.cu", [ "--tid1"; "0"; "--tid2"; "1" ], 1);
    (* Data-race free example *)
    ("drf-shared-mem.cu", [], 0);
    (* Shared memory *)
    ("racy-shared-mem.cu", [], 1);
    (* Shared memory in a device function *)
    ("racy-shared-mem-2.cu", [], 1);
    (* Data-race free example with array aliasing *)
    ("drf-alias.cu", [], 0);
    (* Data-race free example with array aliasing *)
    ("racy-alias.cu", [], 1);
    (* Data-race with atomics. *)
    ("racy-atomics.cu", [], 1);
    (* Atomic-3: atomicCAS winner-uniqueness. The hardware guarantees
       at most one thread per address sees [old == SENTINEL], so the
       conditional write at the same slot is DRF even when [loc]
       could otherwise collide. *)
    ("drf-cas-winner.cu", [], 0);
    (* Negative companion: same kernel shape but the seed read is
       plain (not atomicCAS), so no winner contract applies. *)
    ("racy-cas-no-winner.cu", [], 1);
    (* Atomic-2: atomicAdd unique-slot. Same-cell, nonzero literal
       delta gives distinct return values across threads, so the
       downstream slot write is DRF. *)
    ("drf-atomicadd-slot.cu", [], 0);
    (* Same contract for a negative literal delta. *)
    ("drf-atomicadd-slot-neg.cu", [], 0);
    (* Zero delta is excluded from the contract: every thread sees
       the same returned value, so the downstream slot write
       aliases. *)
    ("racy-atomicadd-zero.cu", [], 1);
    (* Per-thread counter cells: distinctness only holds when threads
       atomic-mod the same address; with disjoint cells two threads
       can both get return 0 and alias on the slot write. *)
    ("racy-atomicadd-per-thread-counter.cu", [], 1);
    (* The atomic scope decides which threads the hardware serialises,
       and [Gen.mode_spec] reads it off the access mode. Two atomics of
       the same scope never conflict at block level, whichever scope
       they carry. *)
    ("atomic-device-scope.cu", [], 0);
    ("atomic-block-scope.cu", [], 0);
    (* At grid level the two scopes part ways: a device-scoped atomic
       still serialises against every thread, while a block-scoped one
       does not serialise against a thread of another block. The
       explicit --gridDim=2 matters, since the default single-block
       grid admits no second block and both kernels come out DRF for
       want of a racing partner rather than by the mode rule. *)
    ("atomic-device-scope.cu", [ "--grid-level"; "--gridDim=2" ], 0);
    ("atomic-block-scope.cu", [ "--grid-level"; "--gridDim=2" ], 1);
    (* An operation with no cross-thread contract is still a memory
       access. atomicExch says nothing about its returned value, but
       two of them on one cell are serialised, so the kernel is DRF. *)
    ("drf-atomicexch-same-cell.cu", [], 0);
    (* The other half of that rule: an atomic conflicts with a plain
       write to the same cell no matter which operation it is. *)
    ("racy-atomicmax-write.cu", [], 1);
    (* The first end-to-end coverage of a predicate carrying a proof.
       Masking by [n - 1] is the identity on [0, n) exactly when [n]
       is a power of two, so the verdict turns on whether the
       assumption is present. *)
    ("drf-pow2-mask.cu", [], 0);
    ("racy-pow2-mask.cu", [], 1);
    (* A data-race that occurs when we have warp-concurrent semantics *)
    ("racy-reduce.cu", [], 1);
    (* Pre-Volta warp-synchronous halving reduction on a single warp:
       racy under post-Volta independent thread scheduling, DRF under
       --assume-warp-synch. *)
    ("drf-warp-synch-reduce.cu", [ "--block-dim=32" ], 1);
    ( "drf-warp-synch-reduce.cu",
      [ "--block-dim=32"; "--assume-warp-synch" ],
      0 );
    (* Negative companion: a cross-warp data race that must survive
       --assume-warp-synch, since the implicit same-warp barrier does
       not order threads in different warps. *)
    ( "racy-cross-warp.cu",
      [ "--block-dim=64"; "--assume-warp-synch" ],
      1 );
    (* A data-race free example as long as the analysis understands typedefs. *)
    ("drf-typedef.cu", [], 0);
    (* The running example of CAV21 *)
    ("racy-cav21.cu", [], 1);
    (* The fixed running example of CAV21 *)
    ("drf-cav21.cu", [], 0);
    (* A racy example *)
    ("racy-device.cu", [], 1);
    (* A data-race that uses aliasing and templated arrays *)
    ("racy-template-alias.cu", [], 1);
    (* A data-race that uses aliasing and templated arrays *)
    ("racy-template.cu", [], 1);
    (* Conditional assignment to a scalar [flag] decides which of
     two writes a thread issues. After [fix_assigns] hoists the
     conditional [Assign] to a [Decl.unset] in the post-If Seq,
     [encode_assigns] sees [flag] as a fresh free local from the
     join point onward and the prover picks adversarial values
     for [flag] per thread to surface the same-cell write between
     adjacent threads. *)
    ("racy-mutation.cu", [], 1);
    (* Support for enumerates *)
    ("drf-enum.cu", [], 0);
    (* Support for anonymous enumerates named via typedef *)
    ("drf-enum-typedef.cu", [], 0);
    (* Support for enumerates *)
    ("drf-enum-constraint.cu", [], 0);
    (* Enum constants with a computed initializer: cu-to-json pre-folds
     [(1 << 8)] and [ANTIALIAS + 1] into a ConstantExpr wrapped in an
     ImplicitCastExpr/ParenExpr, which parse_init must unwrap to read
     the folded value. *)
    ("drf-enum-computed.cu", [], 0);
    (* PredefinedExpr arguments (__FUNCTION__ / __func__ /
     __PRETTY_FUNCTION__): cu-to-json resolves each to a string
     constant, which parse_expr treats as an opaque unknown value
     like any StringLiteral rather than rejecting with parse_exp. *)
    ("drf-predefined-expr.cu", [], 0);
    (* A [[maybe_unused]] attribute on a local variable emits an
     UnusedAttr node in the VarDecl's inner list; parse_decl must drop
     the attribute rather than treating it as an initializer. *)
    ("drf-maybe-unused.cu", [], 0);
    (* sizeof...(pack) in a dependent template body reaches the reader as
     a SizeOfPackExpr, whose count is unknown until instantiation and is
     read as an opaque unknown value. *)
    ("drf-sizeof-pack.cu", [], 0);
    (* [x & 1] tests the low bit of [x], so it holds for the odd values of
     [x] and fails for the even ones. Both polarities are pinned, and each
     is pinned twice, once over a signed [i] and once over an unsigned one,
     because the encoding fixes the remainder's signedness rather than
     taking it from the operand. blockDim.x = 3 makes the two guards admit
     sets of different size, {1} against {0, 2}, so the verdicts differ:
     one author for y[0] against two. *)
    ("drf-and1-odd.cu", [ "--blockDim=3" ], 0);
    ("racy-and1-even.cu", [ "--blockDim=3" ], 1);
    ("drf-and1-odd-unsigned.cu", [ "--blockDim=3" ], 0);
    ("racy-and1-even-unsigned.cu", [ "--blockDim=3" ], 1);
    (* [sizeof(int)] must be the width of the operand, 4, not the width of
     the trait's own result type, [unsigned long]. Using the index modulus
     [threadIdx.x % sizeof(int)] makes the difference observable: at
     blockDim.x = 8 a modulus of 4 collides threads 0 and 4, whereas a
     modulus of 8 would leave every index distinct. *)
    ("racy-sizeof-mod.cu", [ "--blockDim=8" ], 1);
    (* A device function bound to a function-pointer template parameter
     reaches the reader as a TemplateArgument carrying a FunctionDecl;
     the argument is read by name (TArgDecl). *)
    ("drf-template-fn-arg.cu", [], 0);
    (* A generic lambda ([&](auto v){...}) invoked directly. Two things
     must work: the LambdaExpr reader descends through the
     FunctionTemplateDecl that wraps a generic lambda's operator(), and
     Lift_lambdas rewrites the operator() call site (a CXXOperatorCallExpr
     whose first argument is the closure) to the synthetic kernel so the
     body's accesses are analysed. Here the body writes [d[v]] with v the
     per-thread argument, so each thread writes a distinct cell: DRF. *)
    ("drf-generic-lambda.cu", [], 0);
    (* A block-scope namespace alias ([namespace a = b;] inside a kernel
     body) is a NamespaceAliasDecl in the DeclStmt, which has no [type]
     and no runtime effect; parse_decl skips it rather than failing on
     the missing field. *)
    ("drf-namespace-alias.cu", [], 0);
    (* A block-scope using-declaration ([using foo::bar;]) is a UsingDecl
     with no [type], from the same no-runtime-effect family; parse_decl
     skips any typeless block-scope decl. *)
    ("drf-using-decl.cu", [], 0);
    (* Same lambda shape but the body writes [d[0]] from every thread, so
     the invocation collides: racy. Pinning that the lambda body is
     actually analysed, not dropped (a dropped body would false-negative
     as DRF). *)
    ("racy-generic-lambda.cu", [], 1);
    (* Aliasing using shared memory (example 1) *)
    ("racy-alias-shmem1.cu", [], 1);
    (* Aliasing using shared memory (example 1) *)
    ("racy-alias-shmem2.cu", [], 1);
    (* Aliasing using shared memory (example 1) *)
    ("racy-alias-shmem3.cu", [], 1);
    (* Aliasing with increment *)
    ("racy-alias-assign.cu", [], 1);
    (* Array accesses of local memory should not introduce data-races. *)
    ("drf-local-array.cu", [], 0);
    (* Check support for macros *)
    ("macro.cu", [ "-DMACRO=" ], 0);
    ("macro.cu", [ "-DMACRO=+ 0" ], 0);
    ("macro.cu", [ "-DMACRO=+ 1" ], 1);
    (* data-race *)
    ("macro.cu", [ "-DMACRO" ], 2);
    (* expands to 1, which is a syntax error *)
    ("macro.cu", [], 2);
    (* syntax error if the macro is not defined *)
    (* A conditional break is inferred as an assertion *)
    ("drf-assert-loop.cu", [], 0);
    (* Two calls to a forward-declared [__device__ unsigned int
     get(int)] must each produce a distinct unknown-valued local.
     The [__is_thread_unif] asserts pin both as thread-uniform, so
     the race witness picks adversarial values that make
     [y[i + offset1]] and [y[i + offset2]] coincide across two
     threads. *)
    ("racy-funcion-call-unknowns.cu", [], 1);
    (* Bug from generating unknowns from a kernel call *)
    ("racy-kernel-calls-return.cu", [], 1);
    (* (int j = 0; j < n; j++) *)
    ("drf-loop1.cu", [], 0);
    (* (int j = n; j >= 0; j--) *)
    ("drf-loop2.cu", [], 0);
    (* (int i = 0; i <= 4; i++) *)
    ("drf-loop3.cu", [], 0);
    (* (int i = 4; i - k; i++) *)
    ("drf-loop4.cu", [], 0);
    (* (int j = n; j > 0; j--) *)
    ("drf-loop5.cu", [], 0);
    (* (int j = 1; j + k < n; j++) *)
    ("drf-loop6.cu", [], 0);
    (* Literal-stride [+= blockDim.x * 2] loop with a [/ 2]
     access mirroring a [(half2* )src] cast. Drives
     [Range.normalize] / [Unsynced.normalize_loops]: the modulo
     stride constraint [(y - 2*tid) % 128 == 0] would otherwise
     combine with [y / 2] into a goal Z3's non-linear-int tactic
     cannot decide; normalization substitutes the iteration
     variable by [2*tid + 128*q] and the goal becomes linear in
     [(tid, q)]. *)
    ("drf-loop-stride-half-cast.cu", [ "--all-dims"; "--assume-dims" ], 0);
    (* Body-top affine induction: [int base = tid; for (...) {
     access(base); base += stride; }]. Each thread writes its own
     column. The body-top [base += stride] is harvested into
     [other_incs] and [extract_incs] prepends a closed-form
     [Decl base = iters * stride + base_init] shadow at the top
     of the loop body; the original [Assign] stays in place and
     mutates the shadow within each iteration. *)
    ("drf-loop-body-induct.cu", [], 0);
    (* Same pattern as above, but the [base += stride] sits in
     the middle of the body — followed by an unrelated per-thread
     write. The harvest fires regardless of position, since the
     shadow gives every iteration the right value via the
     substitution encoder. *)
    ("drf-loop-body-induct-mid.cu", [], 0);
    (* Two sequential loops sharing a body-mutated induction
     variable: the first loop's exit value of [base] is propagated
     to the second loop via [For.post_for_assigns] (final-value
     replacement, SCEV-style). Without that propagation the second
     loop's shadow would start from [base]'s pre-first-loop value,
     and Z3 would witness a fake cross-loop same-column collision. *)
    ("drf-loop-body-induct-final-value.cu", [], 0);
    (* (int j = 0; j <= n; j++) *)
    ("racy-loop1.cu", [ "-p"; "n=0"; "--index=[0]" ], 1);
    (* (int j = n; j >= 0; j--) *)
    ("racy-loop2.cu", [ "-p"; "n=1"; "--index=[1]" ], 1);
    (* the comma operator *)
    ("racy-comma.cu", [], 1);
    (* the comma operator *)
    ("drf-comma.cu", [], 0);
    (* index of a templated type *)
    ("drf-template-index.cu", [], 0);
    (* Each launch of a templated kernel produces its own specialisation
     alongside the primary template; every specialisation must be
     parsed as a separate kernel, not collapsed into the primary. *)
    ("drf-template-instances.cu", [], 0);
    (* A read hoisted out of a ?: keeps the ternary condition as a guard,
     so the guarded read of s[tid] stays disjoint from the write to
     s[tid-1]; without the guard the read is unconditional and races. *)
    ("drf-ternary-guarded-read.cu", [], 0);
    (* Variadic-template kernel: the [vals...] parameter-pack expansion
     in the primary template body must be preserved through parsing
     rather than collapsed away. *)
    ("drf-template-pack.cu", [], 0);
    (* Variadic-template kernel with explicit launches generating
     [variadic<int>] and [variadic<int, int>] specialisations. The
     resolved template arguments must reach faial as a pack-shaped
     TemplateArgument whose elements are the individual concrete
     types. *)
    ("drf-template-pack-instances.cu", [], 0);
    (* Templated kernel writing [Traits<T>::value] to a single shared
     index from every thread. With no explicit launch, the primary
     template body is parsed and the qualified dependent reference
     reaches the analyser as a [DependentScopeRef] rather than
     collapsing to RecoveryExpr. *)
    ("racy-template-dep-scope.cu", [], 1);
    (* Launch metadata: one [<<<grid, block>>>] launch with host-side
     dim3 locals and a templated kernel argument. The LaunchParam node
     emitted alongside the AST must parse without disturbing the
     kernel-level DRF analysis. *)
    ("drf-launch-param.cu", [], 0);
    (* --assume-launch must rescue an under-constrained kernel that is
     racy when blockDim/gridDim's [y]/[z] axes are free: synthesising
     the launch's [dim3((n+255)/256)] / [dim3(256)] pins the unused
     axes to 1 via [assert(...)] in the pseudo-kernel body. *)
    ("drf-launch-rescue.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* Two distinct launches of the same templated kernel must each
     produce their own pseudo-kernel and analyse independently with
     the launch's concrete dims. *)
    ("drf-launch-multi.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* Negative control: a kernel that races regardless of launch
     dims (every thread writes [out[0]]) stays racy under
     [--assume-launch] — pinning blockDim doesn't suppress real
     races. *)
    ("racy-launch-mismatch.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 1);
    (* A scalar kernel arg supplied by a non-Ident launch-site
     expression (here [params[0]]). The launch-arg resolver folds
     the array-subscript into a fresh uniform pseudo-parameter so
     the formal stays block-uniform; analyses DRF. Without the
     resolver, this would false-positive racy because the launch
     arg surfaces as a per-thread @AccessState. *)
    ("drf-launch-complex-arg.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* A scalar kernel arg fed by a [const int N = 256] host
     variable that c-to-json const-folds to its literal value at
     the launch site. The resolver passes literals through as
     [Const] so the inliner substitutes the kernel formal with
     [256] directly. Without pass-through, the formal stays
     unbound and Z3 picks an adversarial witness, false-positive
     reporting racy on a kernel where every (blockIdx.x,
     threadIdx.x) pair writes a distinct address. *)
    ("drf-launch-const-arg.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* A structured-binding variable ([const auto [slot, token] = ...])
     passed as a scalar launch argument. The launch-arg walk reads the
     [slot] reference, whose [DeclRefExpr] resolves to a [BindingDecl];
     that decl kind is read like a [VarDecl]. Without it the launch
     synthesis hits parse_exp and the whole file exits 2. *)
    ("drf-launch-binding-arg.cu",
     [ "--all-dims"; "--assume-launch" ], 0);
    (* Grid-arithmetic relation flowing transitively to a kernel
     arg: launch picks [gridDim.x = imageW / 128]. The launch-arg
     resolver passes the BinaryOp structure through verbatim
     (Const path), so the assertion [gridDim.x == imageW / 128]
     reaches Z3, which derives [imageW >= 128] transitively from
     the existing [gridDim.x >= 1] preamble. Without
     pass-through (when the resolver abstracts [imageW / 128]
     into a fresh uniform), Z3 has no link between [gridDim.x]
     and [imageW], witnesses [imageW < 128], and false-positive
     reports racy. *)
    ("drf-launch-grid-arith.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* Host-side guard ([if (n >= 256)]) enclosing the launch
     reaches the analyser via c-to-json's [path_condition] slot;
     the synth kernel lifts it into [assert(n >= 256)] alongside
     the dim asserts. The kernel races without the bound (a
     stride pattern: two threads in different blocks collide
     when [n < blockDim.x]); with the lifted path condition Z3
     rules out small [n] and the kernel verifies DRF. *)
    ("drf-launch-path-cond.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* Host-side const-binding ([const int inum = N * 1024]) used
     nested in the grid axis ([dim3(inum / 256)]) reaches the
     analyser via c-to-json's [const_bindings] slot. The synth
     kernel lifts each binding into a local [const int <name> =
     <init>;] decl, which [d_to_imp] lowers to a definitional
     binding in Imp. Combined with [assert(gridDim.x == inum /
     256)] and [gridDim.x >= 1], Z3 derives [N >= 1] transitively
     and the stride-pattern kernel verifies DRF. Without the
     binding lift, [inum] is a free uniform with no tie to [N],
     Z3 picks [N == 0], and the kernel false-positive reports
     racy. *)
    ("drf-launch-const-binding.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* Opaque-block launch: the launch supplies a struct field of
     type [dim3] as the block dim, which cu-to-json wraps in a
     copy-ctor [CXXConstructExpr] whose first arg is itself
     [dim3]-typed. The launch-arg resolver does not decompose that
     shape and emits no per-axis assert for [blockDim], leaving the
     dims universally quantified under [--all-dims]. The kernel
     writes via [atomicInc], DRF regardless of contention. Pins
     that the "no constraint" output for opaque axes doesn't lose
     legitimate DRF cases. *)
    ("drf-launch-opaque-block.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* Opaque-block launch on a kernel whose index uses only
     [threadIdx.x] and thus races when [blockDim.y]/[.z] can exceed
     1. With the resolver emitting no constraint on the opaque
     axes, [blockDim.y] stays free under [--all-dims] and Z3
     witnesses a race between two threads differing on
     [threadIdx.y]. A prior implementation fabricated
     [blockDim.y == 1] / [blockDim.z == 1] for this case,
     suppressing the race (false-negative DRF). *)
    ("racy-launch-opaque-block.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 1);
    (* 2d array *)
    ("drf-2d.cu", [], 0);
    (* add support for side-effects (reads/writes) in the conditions as commas *)
    ("drf-loop-comma-in-cond.cu", [], 0);
    ("racy-loop-comma-in-cond.cu", [], 1);
    (* support for inlining functions which return values *)
    ("drf-inline-var.cu", [], 0);
    (* if-conversion of a conditional scalar assignment: [idx] is reassigned
     inside an [if], and its post-branch value is [flag ? i + n : i], injective
     in the thread id on both arms, so the write is DRF. Without if-conversion
     [idx] is dropped after the branch and the write aliases. *)
    ("drf-cond-assign.cu", [], 0);
    (* Blowup guard for if-conversion: a chain of conditional self-updates
     [if (i < k) i = i + n;] each if-converts to [i = (i < k) ? i + n : i],
     referencing [i] on every arm, so inlining the chain expands the write index
     to a term exponential in the chain length (16 levels timed out / exhausted
     memory before SMT). Encode_assigns bounds the inlined size
     ([--infer-cond-bound]) and abstracts an over-budget value to an unknown, so
     this stays flat; the index is then unconstrained, hence racy. *)
    ("racy-cond-assign-chain.cu", [], 1);
    (* Companion guard for a non-conditional chain: a loop-carried scalar [s]
     repeatedly self-multiplied [s = s * s * v] doubles the inlined term at every
     step. The same inlined-size bound abstracts [s] once it exceeds the budget,
     so this stays flat. *)
    ("drf-loop-mul-chain.cu", [], 0);
    (* Regression for a bug in [drf/lib/delinearize.ml]'s
     [Expr.( - )] polynomial-subtraction primitive that mis-signed
     remainder terms when delinearising indices containing
     [Minus] subexpressions. The kernel shape mirrors the
     per-layer loop of an in-place square matrix rotation. *)
    ("drf-delin-rotate-bug.cu", [], 0);
    (* A synchronized grid-stride loop whose start [idx = blockIdx.x *
     blockDim.x + threadIdx.x] is thread-dependent, so the outer loop
     variable is thread-varying in value even though the loop is
     well-formed (uniform trip count). Under [--delin-algo cramer
     --assume-delin], delinearization must classify that loop variable
     as thread-varying, not uniform: otherwise the Flag shape inference
     promotes it to an array dimension and drops it from the subscript,
     so every thread appears to write the same cell and faial reports a
     false race. *)
    ("drf-delin-gridstride.cu",
     [ "--all-dims"; "--assume-dims"; "--assume-launch";
       "--assume-delin"; "--delin-algo"; "cramer" ], 0);
    (* Each thread writes to a unique cell of [arr]. The callee
     [f] has a local [int i;] whose name collides with the
     caller's [i]; faial-drf's parameter-substitution path under
     the inliner had previously bound the formal [p] against the
     alpha-renamed callee local instead of the call-site
     [arr + i]. *)
    ("drf-pointer-param-shadow.cu", [], 0);
    (* Regression: [Imp.Scoped.Code.vars_distinct] must pick fresh
     names that avoid binders living deeper in the body, not just the
     names bound on the path from the root. A helper's parameter [i]
     inlined into a kernel with a clashing local [i] once got renamed
     to a fresh name that collided with a deeper binder, and the
     resulting capture landed the access on the wrong local, reporting
     a false race. *)
    ("drf-inline-rename-capture.cu",
     [ "--all-dims"; "--all-levels"; "--assume-launch" ], 0);
    (* ensure that an aligned protocol remains aligned *)
    ("drf-loop-aligned-1.cu", [], 0);
    (* End-to-end smoke test for IntegerLiteral parsing of uint64
     sentinels that exceed OCaml's 63-bit int — they must reach the
     analyser as concrete two's-complement values, not the
     [Int.max_int] fallback. *)
    ("drf-uint64-sentinel.cu", [], 0);
    (* A 64-bit parameter's bound is a hypothesis about an argument, and
       neither end of a signed 64-bit domain is an OCaml int. Answering
       with the 32-bit signed range rules out every argument beyond
       2147483647, which is exactly where this kernel's race lives: the
       verdict was data-race free and the witness now picks
       [n = 3000000001]. *)
    ("racy-int64-param.cu", [], 1);
    (* The half of that domain that is still writable. An unsigned 64-bit
       parameter keeps [n >= 0], which is what makes this companion's
       [n + 1 == 0] branch unreachable; dropping both ends instead of the
       upper one alone witnesses [n = -1] and reports a race the kernel
       does not have. *)
    ("drf-uint64-param.cu", [], 0);
    (* C++11 range-based for over a fixed-size array: the bound is
     extracted from the RangeStmt's qualType so the iteration
     variable becomes [arr[__idx]] inside a bounded foreach,
     instead of an unbounded Star. *)
    ("drf-range-for.cu", [], 0);
    (* C++ [while (auto i = n) { ...; i--; }]: clang emits a 3-item
     [DeclStmt; cond; body] inner array. The parser lowers it to
     [for (auto i = n; i; ) body], keeping [i] as the loop's own
     binding; the step is inferred from [i--] in the body. *)
    ("drf-while-decl.cu", [], 0);
    (* Pointer parameter with [volatile T * const __restrict]
     qualifier stack: c_type's pointer detection must normalise the
     trailing qualifier soup so the parameter classifies as a
     global array. Without normalisation, the parameter is
     Unsupported and every access lowers to [skip], producing a
     false-negative DRF on a kernel that races on every thread. *)
    ("racy-qualified-pointer.cu", [], 1);
    (* Congruence on the read symbol doing real work: with one warp per
     block both threads of a witness load the same cell and share a
     base, so the write index separates them by thread id alone. The
     two load indices are distinct terms, one per thread, so only the
     equality axiom relates them. *)
    ("drf-read-congruence.cu", [ "--block-dim=32" ], 0);
    (* Two warps per block put the witness on unrelated bases, which
     can undercut each other by the thread-id gap. *)
    ("drf-read-congruence.cu", [ "--block-dim=64" ], 1);
    (* A load that follows a store to the same array must not reuse the
     earlier load's value: reading a cell back after overwriting it
     yields a different number, and cancelling the two against each
     other clears a real race. *)
    ("racy-read-after-write.cu", [], 1);
    (* The companion precision case: separating the two loads must not
     downgrade either to an unknown local. Both stay uniform across
     threads, so the write index offsets a shared constant by the
     thread id. *)
    ("drf-read-version.cu", [], 0);
    (* A read symbol is minted only after an array has reached its final
     name and its index has picked up every offset folded into it. A load
     in a callee therefore carries the caller's array and the offset of
     the argument, and agrees with a load the caller writes directly on
     that array, so the two cancel in the write index. Minting the symbol
     before inlining keeps the callee's parameter as the array and drops
     the offset, which separates the two loads and reports a race. *)
    ("drf-read-call-arg.cu", [], 0);
    (* The other direction on the offset: two calls of one callee at
     different offsets of one array read different cells and must stay
     apart, so their difference is free and two threads collide. Dropping
     the offset of the argument merges the two loads and clears the
     race. *)
    ("racy-read-call-offsets.cu", [], 1);
    (* A pointer bound to the interior of an array is resolved before the
     symbol is minted, so a load through the pointer and a load written on
     the array itself are the same load. *)
    ("drf-read-source-alias.cu", [], 0);
    (* Offsets accumulate along a chain of calls, and the load carries the
     sum of all of them. *)
    ("drf-read-nested-call.cu", [], 0);
    (* One callee inlined against two arrays keeps the arrays apart, since
     the symbol is named after the resolved array. A symbol shared by every
     array, or one left named after the callee's parameter, merges the two
     loads and clears the race. *)
    ("racy-read-two-arrays.cu", [], 1);
    (* An uninterpreted function's result carries the range its
     declaration states. Without it __clz is an unbounded integer and a
     stride of 64 does not separate two threads, so the kernel reports a
     race it does not have. *)
    ("drf-clz-range.cu", [], 0);
    (* The companion: at a stride of 16 the same range no longer
     separates the threads, so the range must not clear this one. *)
    ("racy-clz-range.cu", [], 1);
    (* The second declared range, against __ffs. *)
    ("drf-ffs-range.cu", [], 0);
    (* Reads are the second class of uninterpreted function, and their
     result range comes from the array's element type. Without it the
     loaded unsigned char is an unbounded integer and the stride of 256
     does not separate two threads. *)
    ("drf-read-elem-range.cu", [], 0);
    (* The companion at an int element type, whose range spans more than
     the stride, so the range must not clear this one. *)
    ("racy-read-elem-range.cu", [], 1);
    (* An entry with a body lowers to that body at every application,
     not only where every argument is a literal. Leaving min an
     uninterpreted function makes its result an unbounded integer, which
     is the same false alarm a missing declaration produces. *)
    ("drf-min-rewrite.cu", [], 0);
    (* The companion: the body has to be min's own graph, since
     rewriting the call to its second argument, to a constant or to max
     clears this one. *)
    ("racy-min-rewrite.cu", [], 1);
    (* The same rung against max, whose body is the mirror conditional. *)
    ("drf-max-rewrite.cu", [], 0);
    ("racy-max-rewrite.cu", [], 1);
    (* The third entry with a body, and the one whose body divides. *)
    ("drf-divup-rewrite.cu", [], 0);
    (* A registered name applied at an arity the entry does not declare
     is a different function: it gets neither the body nor the
     declaration, and applying the body regardless raises out of it. *)
    ("racy-divup-arity.cu", [], 1);
    (* The third entry carrying a 0..32 result, whose companion is
     racy-clz-range.cu. *)
    ("drf-popc-range.cu", [], 0);
    (* The 64-bit intrinsics count over a wider word, so their result
     range is 0..64 and a stride of 128 is what separates two threads. *)
    ("drf-clzll-range.cu", [], 0);
    (* The companion at a stride of 64, the exact width of that range,
     which fails if the 64-bit entries inherit the 32-bit range. *)
    ("racy-clzll-range.cu", [], 1);
    (* The other two 64-bit entries, sharing that companion. *)
    ("drf-ffsll-range.cu", [], 0);
    ("drf-popcll-range.cu", [], 0);
    (* A declaration is instantiated once per occurring application, and
     an application nested inside another is still one of them. Dropping
     either of the two ranges reports a race. *)
    ("drf-nested-range.cu", [], 0);
    (* The companion at a stride of 64, the two ranges summed. *)
    ("racy-nested-range.cu", [], 1);
    (* A declaration governs every query, not only the race query. The
     precondition here contradicts __clz's declared range, so the kernel
     clears vacuously and --check-pre-sat must say so. A precondition
     query that does not carry the declaration finds the precondition
     satisfiable, runs the race pipeline and reports plain data-race
     freedom, which is exit 0 rather than the 1 expected here. *)
    ("vacuous-clz-pre.cu",
     [ "--check-pre-sat"; "--assume"; "__clz(n) > 40" ], 1);
    (* An integer conversion carries a term of its own from clang's AST down
       to [Exp.nexp]. These four pin that the term arrives and that it does
       not yet prove anything: every consumer treats it as its operand, so
       the verdicts are the ones faial gave when the parser discarded the
       conversion.

       The two below are the implicit and explicit syntaxes for the same
       operation, and both build the identical protocol
       [rw out[(char)(((int)threadIdx.x) * 256)]]. Both verdicts are wrong:
       the low eight bits of a multiple of 256 are zero, so every thread
       writes out[0]. They flip to racy once the node is given a meaning in
       the solver, which is what makes them worth asserting now. *)
    ("drf-cast-implicit.cu", [], 0);
    ("drf-cast-explicit.cu", [], 0);
    (* A conversion over a load. Reads are hoisted to statements before
       [Infer_exp], so the node has to wrap the value the load was bound to
       rather than the load expression; it comes out as
       [rw out[(char)$read_a(0, (int)threadIdx.x)]]. Racy because the loaded
       value is unconstrained beyond its element range, which is the verdict
       from before the node existed. *)
    ("racy-cast-read.cu", [], 1);
    (* The third syntax a conversion arrives through, and the one this node
       is not for: [__float2int_rz] is declared [__device__ int
       __float2int_rz(float)], so its call site is a CallExpr with no cast
       node and no two types to compare. It mints nothing and stays an
       unknown value. Reaching it is the [Functions] registry's job. *)
    ("racy-cast-intrinsic.cu", [], 1);
    (* A literal stored into a narrow-element array is [int -> char], which
       the keep-rule keeps, so [D_lang]'s benign-write payload has to read
       through the conversion. Losing it drops the [rw(0)] tag, and a write
       with no payload is never paired off as benign, which turns this DRF
       kernel racy. *)
    ("drf-cast-narrow-store.cu", [], 0);
    (* A right shift rounds towards minus infinity and keeps the operand's
       signedness, and these four pin both halves of that on the three paths
       a shift can take through the folder.

       Both operands literal: the answer is settled while folding, and a
       negative left operand shifted as a machine-word logical shift comes
       back a large positive number, which closes the guard and loses the
       race. *)
    ("racy-shift-literal-negative.cu", [], 1);
    (* Literal shift amount over a value that may be negative. Rewriting the
       shift into a division that truncates towards zero answers 0 for
       n = -1, where the shift answers -1. The [&] in the index is what puts
       this on the bit-vector backend, since the arithmetic encoder has no
       bitwise operators; that backend is where truncation and flooring
       differ. *)
    ("drf-shift-signed-floor.cu", [], 0);
    (* Non-literal shift amount over an unsigned operand, whose marker has to
       survive folding: an unsigned shift fills zero, so an all-ones value
       shifted by 1..63 is no longer all ones. Carrying it as a signed shift
       fills with the sign and opens the branch. *)
    ("drf-shift-unsigned-fill.cu", [], 0);
    (* The other direction on the fill bit: a signed shift keeps the sign, so
       a negative operand stays negative and the race is real. Guards against
       answering every shift with a zero fill. *)
    ("racy-shift-signed-keeps-sign.cu", [], 1);
  ]

(* These are kernels that are being documented, but are
   not currently being checked *)
let unsupported : Fpath.t list =
  [
    "drf-warp.cu";
    "racy-warp.cu";
    "racy-device-ref.cu";
    (* example where assignment is used as an expression, rather
     than a statement *)
    "drf-assign-exp.cu";
    (* Data-race free requires understanding fields in parameters. *)
    "drf-field-in-param.cu";
    (* A racy example that uses structs *)
    "racy-struct.cu";
    (* A racy example that calls a device function without array as args *)
    "racy-device-no-args.cu";
  ]
  |> List.map (fun x -> Fpath.(v "." / x))

(* ---- Testing-specific code ----- *)

(* Get the absolute path of the test binary *)
let test_exe : Fpath.t = Fpath.(v Sys.executable_name |> normalize)

(* Get the absolute path of the test binary *)
let test_dir : Fpath.t = Fpath.(test_exe |> parent)

(* Get the absolute path of the build directory *)
let build_dir : Fpath.t = Fpath.(test_dir |> parent |> parent |> normalize)

(* Get the absolute path of the root of our project *)
let workspace_dir : Fpath.t = Fpath.(build_dir |> parent |> parent)

(* Get the path of faial-drf *)
let faial_drf_exe : Fpath.t = Fpath.(build_dir / "drf" / "bin" / "main.exe")

let faial_drf ?(args = []) (fname : Fpath.t) : Subprocess.t =
  Subprocess.make
    (Fpath.to_string faial_drf_exe)
    (args @ [ fname |> Fpath.to_string ])

let used_files : Fpath.Set.t =
  tests
  (* get just the filenames as paths *)
  |> List.map (fun (x, _, _) -> Fpath.(v "." / x))
  (* convert to a set *)
  |> Fpath.Set.of_list

let missed_files (dir : Fpath.t) : Fpath.Set.t =
  let all_cu_files : Fpath.Set.t =
    dir |> Files.read_dir
    |> List.filter (Fpath.has_ext ".cu")
    |> Fpath.Set.of_list
  in
  let unsupported = Fpath.Set.of_list unsupported in
  Fpath.Set.diff (Fpath.Set.diff all_cu_files used_files) unsupported

let () =
  let open Fpath in
  print_endline "Checking examples for DRF:";
  Unix.chdir (Fpath.to_string test_dir);
  tests
  |> List.iter (fun (filename, args, expected_status) ->
      let str_args = if args = [] then "" else String.concat " " args ^ " " in
      let bullet =
        match expected_status with
        | 0 -> "DRF:   "
        | 1 -> "RACY:  "
        | 2 -> "PARSE: "
        | _ -> "?:     "
      in
      print_string (bullet ^ "faial-drf " ^ str_args ^ filename);
      Stdlib.flush_all ();
      let given = faial_drf ~args (v filename) |> Subprocess.run_split in
      (if given.status = Unix.WEXITED expected_status then print_endline " ✔"
       else
         let exit_code = Subprocess.exit_code given.status |> string_of_int in
         print_endline " ✘";
         print_endline
           "------------------------ OUTPUT ------------------------";
         print_endline given.stdout;
         print_endline given.stderr;
         print_endline
           ("ERROR: Expected return code "
           ^ string_of_int expected_status
           ^ " but got " ^ exit_code);
         print_endline "";
         (* Get the generated binary *)
         let exe =
           faial_drf_exe
           |> Fpath.relativize ~root:workspace_dir
           |> Option.value ~default:(Fpath.v "faial-drf")
           |> Fpath.to_string
         in
         (* Get the path of the test file *)
         let filename =
           test_dir / filename
           |> Fpath.relativize ~root:workspace_dir
           |> Option.get |> Fpath.to_string
         in
         print_endline "Re-run file:";
         print_endline
           (" - " ^ exe ^ " " ^ String.concat " " (args @ [ filename ]));
         let test_exe =
           test_exe
           |> Fpath.relativize ~root:workspace_dir
           |> Option.get |> Fpath.to_string
         in
         print_endline "Re-run test:";
         print_endline (" - dune exec " ^ test_exe);
         exit 1);
      Stdlib.flush_all ());
  unsupported
  |> List.iter (fun f ->
      if not (Files.exists f) then (
        print_endline ("Missing unsupported file: " ^ Fpath.to_string f);
        exit 1)
      else print_endline ("TODO:  " ^ Fpath.to_string f));
  let missed = missed_files (v ".") in
  if not (Fpath.Set.is_empty missed) then (
    let missed =
      missed |> Fpath.Set.to_list |> List.sort Fpath.compare
      |> List.map Fpath.to_string |> String.concat " "
    in
    print_endline "";
    print_endline ("ERROR: The following files are not being checked: " ^ missed);
    exit (-1))
  else ()
