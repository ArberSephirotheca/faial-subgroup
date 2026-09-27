__constant__ int *table[4];

__global__ void k() {
  __shared__ int tile[64];
  tile[0] = threadIdx.x;
  int *row = table[0];
  row[threadIdx.x] = threadIdx.x;
}
