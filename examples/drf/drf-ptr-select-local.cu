__global__ void k(int *A, int *B, int c) {
  int *p = c ? A : B;
  p[threadIdx.x] = threadIdx.x;
}
