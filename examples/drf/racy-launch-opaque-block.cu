// A kernel index that uses only [blockIdx.x] and [threadIdx.x]
// races when [blockDim.y] (or [.z]) can exceed 1, since two threads
// differing only on [threadIdx.y] compute the same address. The
// launch supplies [c.b] — a struct field of type [dim3] — as the
// block dim. cu-to-json wraps the field reference in a copy-ctor
// [CXXConstructExpr] whose first arg is itself [dim3]-typed; the
// launch-arg resolver does not decompose that shape into integer
// axes. Under [--assume-launch], no per-axis assert is emitted for
// [blockDim], so under [--all-dims] the dims stay universally
// quantified and Z3 witnesses [blockDim.y > 1].
//
// A prior implementation fabricated [blockDim.y == 1] and
// [blockDim.z == 1] for the opaque-axis case, silently rescuing
// kernels that genuinely race when [y]/[z] are free — a
// false-negative DRF. This fixture pins that the resolver returns
// no constraint on opaque axes rather than inventing literal [1]s.
struct Cfg { dim3 b; };

__global__ void racy_opaque_block(int n, int *y) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) y[i] = threadIdx.x;
}

void run(int n, int *y) {
    Cfg c;
    c.b = dim3(256);
    racy_opaque_block<<<dim3((n + 255) / 256), c.b>>>(n, y);
}
