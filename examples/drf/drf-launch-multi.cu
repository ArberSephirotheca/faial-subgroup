// One templated kernel launched from two host functions with disjoint
// concrete dims (small: 128 threads/block, large: 1024). With
// --assume-launch, two pseudo-kernels are synthesised and analyzed
// independently; each binding pins the surrounding gridDim/blockDim
// to its launch's dims. Both must be DRF.
template <typename T>
__global__ void saxpy_multi(int n, T a, T *x, T *y) {
    int i = blockIdx.x * blockDim.x + threadIdx.x;
    if (i < n) y[i] = a * x[i] + y[i];
}

void run_small(int n, float a, float *x, float *y) {
    saxpy_multi<float><<<dim3((n + 127) / 128), dim3(128)>>>(n, a, x, y);
}

void run_large(int n, float a, float *x, float *y) {
    saxpy_multi<float><<<dim3((n + 1023) / 1024), dim3(1024)>>>(n, a, x, y);
}
