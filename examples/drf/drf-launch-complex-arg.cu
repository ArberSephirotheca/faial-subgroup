// A scalar kernel argument supplied at the launch site by an
// array-subscript expression (here, params[0]) rather than a bare
// host-side identifier.
//
// Without launch-arg resolution, the launch-site argument is routed
// through the kernel-code rewriter and surfaces as a per-thread
// @AccessState; the SMT solver then assigns distinct values for [n]
// across threads and reports a false-positive race on y[i].
//
// With launch-arg resolution, the array-subscript collapses into a
// fresh uniform pseudo-parameter — block-uniform by construction —
// and the kernel analyses DRF under --all-dims --all-levels
// --assume-launch.
__global__ void saxpy_complex_arg(int n, float a, float *x, float *y) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) y[i] = a * x[i] + y[i];
}

void run(int *params, float a, float *x, float *y) {
    saxpy_complex_arg<<<dim3((params[0] + 255) / 256), dim3(256)>>>(
        params[0], a, x, y);
}
