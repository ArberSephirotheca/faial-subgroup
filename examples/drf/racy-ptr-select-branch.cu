__global__ void k(int *A, int *B, int c) {
  int *p;
  if (c) p = A;
  else p = B;
  p[0] = threadIdx.x;
}
