__device__ void touch(int *A, int i);

__device__ void helper(int *A, int i) {
  touch(A, i);
}

__global__ void k(int *A) {
  A[threadIdx.x] = threadIdx.x;
  helper(A, threadIdx.x);
}
