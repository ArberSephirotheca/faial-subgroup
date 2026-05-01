// expected: divergent
//
// Loop count is read from memory into a per-thread decl. The loop guard
// references a local, so the binder is added as projectable; the barrier
// inside is reached on iterations that exist in T1 but not in T2.

__global__ void k(int *data) {
    int n = data[threadIdx.x];
    for (int i = 0; i < n; ++i) {
        __syncthreads();
    }
}
