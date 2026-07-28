__device__ void rec(int i, int *A) {
  if (i <= 0) return;
  rec(i - 1, A);
}

__device__ void helper(int *A) {
  A[0] = threadIdx.x;
  rec(4, A);
}

__global__ void k(int *A) {
  helper(A);
}
