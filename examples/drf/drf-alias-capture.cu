__device__ float A[64];

__global__ void k() {
  int i = threadIdx.x;
  float *base = A + i;
  i = 0;
  base[0] = threadIdx.x;
}
