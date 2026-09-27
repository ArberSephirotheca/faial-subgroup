__constant__ int *table[4];

__global__ void k() {
  int *row = table[0];
  row[blockIdx.x * blockDim.x + threadIdx.x] = threadIdx.x;
}
