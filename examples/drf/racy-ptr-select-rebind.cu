__global__ void k(int *A, int *B, int c) {
  int *p = A;
  if (c) p = B;
  if (threadIdx.x == 0) p[0] = 1;
  if (threadIdx.x == 1) B[0] = 2;
}
