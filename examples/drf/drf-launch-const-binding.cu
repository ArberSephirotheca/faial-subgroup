// Const-binding lifting: a stride-pattern kernel whose stride depends
// on a host-side parameter [N] that is *not* directly tied to the
// launch dims at the syntactic level. The host introduces a const
// local [inum = N * 1024] and uses it nested inside the grid axis
// expression ([dim3(inum / 256)]). Two threads in different blocks
// collide on [(b1 - b2) * N * 1024 == t2 - t1]; with [|t2 - t1| <
// 256 = blockDim.x] that's only solvable when [N == 0].
//
// c-to-json admits [inum] in the launch's [const_bindings] slot
// (host-local [const]-qualified, no address taken, reachable from
// the grid expression). The trivial-init substitution policy is
// top-level only — [inum] inside [inum / 256] is nested, so [inum]
// stays as a [DeclRefExpr] in the grid axis and the binding
// [inum == N * 1024] travels alongside.
//
// The synth kernel lifts the binding into a local
// [const int inum = N * 1024;] decl ahead of the dim asserts.
// [d_to_imp] lowers the decl to a definitional binding in Imp;
// combined with [assert(gridDim.x == inum / 256)] and the existing
// [gridDim.x >= 1] preamble, Z3 derives [N >= 1] transitively, and
// the kernel verifies DRF.
//
// Without the binding lift, [inum] is a free synth-kernel parameter
// with no relation to [N], Z3 picks [N == 0] (the only witness
// where the stride-pattern collides), and the kernel false-positive
// reports racy.
__global__ void const_binding_stride(int N, int *y) {
    y[blockIdx.x * N * 1024 + threadIdx.x] = threadIdx.x;
}

void run(int N, int *y) {
    const int inum = N * 1024;
    const_binding_stride<<<dim3(inum / 256), dim3(256)>>>(N, y);
}
