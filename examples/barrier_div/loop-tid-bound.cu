// expected: missing participants.
//
// Loop bound depends on threadIdx.x. At iteration 0, only threads with
// tid > 0 are still in the loop (31 of 32) — the cohort never matches
// the full block. At iteration 1, 30; etc.

__global__ void k() {
    for (int i = 0; i < threadIdx.x; i++) {
        __syncthreads();
    }
}
