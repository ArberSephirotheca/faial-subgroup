__device__ void touch(int *A, int i) { A[i] = i; }
__device__ void touch(int *A, int i);

__global__ void k(int *A) {
  touch(A, threadIdx.x);
}
