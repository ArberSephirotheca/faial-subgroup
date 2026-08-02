__global__ void k(int *A, int *B, int c) {
  int *p = c ? A : B;
  if (threadIdx.x == 0) p[0] = 1;
  if (threadIdx.x == 1) A[0] = 2;
}
