__global__ void k(int *A) {
  __shared__ int tile[64];
  tile[0] = threadIdx.x;
  A[blockIdx.x * blockDim.x + threadIdx.x] = threadIdx.x;
}
