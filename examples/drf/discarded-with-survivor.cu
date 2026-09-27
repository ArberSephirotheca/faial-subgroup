__device__ void touch(int *A, int i);

__global__ void declined(int *A) {
  touch(A, threadIdx.x);
}

__global__ void analyzed(int *A) {
  A[threadIdx.x] = 0;
}
