__device__ float A[64];

__global__ void k() {
  int i = 0;
  float *base = A + i;
  i = threadIdx.x;
  base[0] = threadIdx.x;
}
