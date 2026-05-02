// Multiple instantiations of a templated kernel: today the C-AST parser
// only sees the primary template (with dependent type T *), and drops the
// reduce<float,128> / reduce<int,256> specialisations that the program
// actually launches. Tracked under feat.md item #1.
template <typename T, int BS>
__global__ void reduce(T *in, T *out, int n) {
    __shared__ T s[BS];
    int tid = threadIdx.x;
    s[tid] = (tid < n) ? in[tid] : T{0};
    __syncthreads();
    for (int stride = BS / 2; stride > 0; stride >>= 1) {
        if (tid < stride) s[tid] += s[tid + stride];
        __syncthreads();
    }
    if (tid == 0) out[blockIdx.x] = s[0];
}

void run(float *fin, float *fout, int *iin, int *iout, int n) {
    reduce<float, 128><<<(n + 127) / 128, 128>>>(fin, fout, n);
    reduce<int,   256><<<(n + 255) / 256, 256>>>(iin, iout, n);
}
