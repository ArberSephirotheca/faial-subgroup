__global__ void k(int *A, int *B, int c) {
  (c ? A : B)[0] = threadIdx.x;
}
