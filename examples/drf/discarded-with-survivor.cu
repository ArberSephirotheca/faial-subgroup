__device__ void touch(int *A, int i);

__global__ void declined(int *A) {
  touch(A, threadIdx.x);
}

__global__ void analysed(int *A) {
  A[threadIdx.x] = 0;
}
