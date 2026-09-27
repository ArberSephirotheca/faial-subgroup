__device__ int score(int i);

__global__ void k(int *A) {
  A[threadIdx.x] = score(threadIdx.x);
}
