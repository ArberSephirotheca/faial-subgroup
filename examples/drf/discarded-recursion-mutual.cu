__device__ void odd(int i, int *A);

__device__ void even(int i, int *A) {
  if (i <= 0) return;
  A[i] = 1;
  odd(i - 1, A);
}

__device__ void odd(int i, int *A) {
  if (i <= 0) return;
  A[i] = 2;
  even(i - 1, A);
}

__global__ void k(int *A) {
  even(threadIdx.x, A);
}
