// Multiple instantiations of a templated kernel. Each launch
// (reduce<float,128>, reduce<int,256>) generates a concrete
// specialisation alongside the primary template; the parser must
// process every specialisation as its own kernel rather than only
// seeing the primary template with its dependent T * parameters.
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
