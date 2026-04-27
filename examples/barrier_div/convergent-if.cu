// expected: no missing-participant errors.
//
// Both branches of the conditional reach the same lexical __syncthreads,
// so the cohorts merge into a full warp before firing.

__global__ void k() {
    if (threadIdx.x < 17) {
        __syncthreads();
    } else {
        __syncthreads();
    }
}
