// expected: missing participants at the barrier.
//
// Only threads with tid.x < 17 reach the __syncthreads(); the remaining
// 15 threads (out of a 32-thread block) skip it. Cohort = 17, expected = 32.

__global__ void k() {
    if (threadIdx.x < 17) {
        __syncthreads();
    }
}
