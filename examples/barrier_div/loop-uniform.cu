// expected: no missing-participant errors.
//
// Barrier inside a loop with a uniform bound. Each iteration fires
// independently; rule L's symbolic-iteration check passes because the
// cohort is the full warp at every iteration.

__global__ void k(int K) {
    for (int i = 0; i < K; i++) {
        __syncthreads();
    }
}
