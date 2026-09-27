__device__ void rec(int i, int *A) {
  if (i <= 0) return;
  A[i] = threadIdx.x;
  rec(i - 1, A);
}

__global__ void k(int *A) {
  A[threadIdx.x] = 0;
  rec(8, A);
}
