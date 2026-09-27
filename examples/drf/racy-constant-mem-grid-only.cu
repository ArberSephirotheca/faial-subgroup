__constant__ int *table[4];

__global__ void k() {
  int *row = table[0];
  row[threadIdx.x] = threadIdx.x;
}
