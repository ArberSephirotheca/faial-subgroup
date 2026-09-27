__global__ void k(int *A, int n) {
  A[threadIdx.x] = min(max(n, 1), 8) + (int)log2((double)n);
}
