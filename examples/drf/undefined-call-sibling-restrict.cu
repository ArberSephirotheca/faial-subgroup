__device__ void touch(int *__restrict__ A, int i) {
  A[i] = i;
}
