// Templated saxpy with no in-source assumptions on blockDim/gridDim.
// Under --all-dims --all-levels (no launch synthesis), faial assumes
// blockDim.{y,z} and gridDim.{y,z} can each exceed 1, so two threads
// differing only in [yz] compute the same index and race on y[i].
// With --assume-launch, the synthesised pseudo-kernel binds the
// launch's [dim3((n+255)/256)] / [dim3(256)] to gridDim/blockDim,
// pinning [y]/[z] to 1 and rescuing the analysis.
template <typename T>
__global__ void saxpy_rescue(int n, T a, T *x, T *y) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) y[i] = a * x[i] + y[i];
}

void run(int n, float a, float *x, float *y) {
    saxpy_rescue<float><<<dim3((n + 255) / 256), dim3(256)>>>(n, a, x, y);
}
