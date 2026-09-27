__device__ void touch(int *A, int i) {
  A[i / 2] = i;
}
