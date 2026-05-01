// expected: no missing-participant errors.
//
// Two distinct lexical barriers in sequence. Each fires uniformly with
// the full cohort.

__global__ void k() {
    __syncthreads();
    __syncthreads();
}
