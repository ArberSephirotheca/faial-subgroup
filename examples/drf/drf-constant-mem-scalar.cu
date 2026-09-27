__constant__ int n = 32;

__global__ void k(int *A) { A[n * threadIdx.x] = threadIdx.x; }
