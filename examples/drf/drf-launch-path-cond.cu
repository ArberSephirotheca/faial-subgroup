// Path-condition lifting: a stride-pattern kernel guarded at the
// launch site by [if (n >= 256)]. Two threads in different blocks
// collide on [b1 * n + t1 == b2 * n + t2] iff [(b1 - b2) * n ==
// t2 - t1]; with [|t2 - t1| < 256 = blockDim.x] that's only
// solvable when [n < 256].
//
// c-to-json emits the enclosing [n >= 256] guard as the
// [path_condition] slot on the launch (its drop rule keeps it
// because [n] is a parameter, not mutated, no calls / members /
// side effects intervene). The synthesised pseudo-kernel lifts
// it into an [assert(n >= 256)] in the body, which becomes an
// SMT hypothesis on every subsequent access — Z3 then rules out
// the [n < 256] witness and the kernel verifies DRF. Without
// the path-condition lift, [n] is unbound and the verifier
// false-positive reports racy.
__global__ void path_cond_stride(int n, int *y) {
    y[blockIdx.x * n + threadIdx.x] = threadIdx.x;
}

void run(int n, int *y) {
    if (n >= 256) {
        path_cond_stride<<<dim3(2), dim3(256)>>>(n, y);
    }
}
