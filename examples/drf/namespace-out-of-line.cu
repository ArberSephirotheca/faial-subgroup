namespace N { __device__ void touch(int *A, int i); }

__global__ void k(int *A) {
  A[threadIdx.x] = threadIdx.x;
  N::touch(A, threadIdx.x);
}
