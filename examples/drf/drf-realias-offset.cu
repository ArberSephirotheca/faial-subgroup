__device__ float A[64];

__global__ void k() {
  float *base = A + 0;
  base[threadIdx.x] = 1;
  base = A + threadIdx.x;
  base[0] = 2;
}
