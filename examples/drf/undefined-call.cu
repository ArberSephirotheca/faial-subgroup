__device__ void touch(int *A, int i);

__global__ void k(int *A) {
  A[threadIdx.x] = threadIdx.x;
  touch(A, threadIdx.x);
}
