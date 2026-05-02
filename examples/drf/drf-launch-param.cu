// Single-launch kernel with host-side dim3 locals and a templated
// kernel parameter. cu-to-json emits a LaunchParam node for the
// [scale<float><<<grid, block>>>(...)] site, with the per-axis
// expressions of grid/block resolved through const-fold and
// trivial-local-init substitution. The parser must consume the
// LaunchParam without disturbing the kernel-level analysis.
template <typename T>
__global__ void scale(T *out, const T *in, int n, T factor) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) out[i] = in[i] * factor;
}

void run(float *o, const float *i, int n) {
    dim3 block(256);
    dim3 grid((n + 255) / 256);
    scale<float><<<grid, block>>>(o, i, n, 2.0f);
}
