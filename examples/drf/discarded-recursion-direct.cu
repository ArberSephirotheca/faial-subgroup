__device__ int rec(int i, int *A) {
  if (i <= 0) return 0;
  A[i] = 1;
  return rec(i - 1, A);
}

__global__ void k(int *A) {
  rec(threadIdx.x, A);
}
