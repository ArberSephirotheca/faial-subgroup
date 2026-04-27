// expected: no missing-participant errors.
//
// Every thread reaches the barrier unconditionally — the trivial case.

__global__ void k() {
    __syncthreads();
}
