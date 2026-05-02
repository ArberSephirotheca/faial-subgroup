// Variadic-template kernel exercising the parameter-pack-expansion
// path. With no explicit instantiation, only the primary template body
// is parsed and the [vals...] expansion is preserved as a wrapper
// around the parameter-pack reference, rather than collapsed away.
template <typename... Ts>
__device__ int sum_pack(Ts...) { return 0; }

template <typename... Ts>
__global__ void variadic_k(int *out, int n, Ts... vals) {
    int tid = threadIdx.x;
    if (tid < n) out[tid] = sum_pack(vals...);
}
