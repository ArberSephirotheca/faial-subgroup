// expected: divergent
//
// Nested case: a tid-only branch (uniform-collapsed) wraps a decl-derived
// branch. Checks that being inside an architectural guard doesn't suppress
// detection of the inner non-determinism.

__global__ void k(int *data) {
    if (threadIdx.x < 32) {
        int x = data[threadIdx.x];
        if (x > 0) {
            __syncthreads();
        }
    }
}
