__device__ int *table[4];

__global__ void k() {
  int *a = table[0];
  int *b = table[0];
  if (threadIdx.x == 0) a[0] = 1;
  if (threadIdx.x == 1) b[0] = 2;
}
