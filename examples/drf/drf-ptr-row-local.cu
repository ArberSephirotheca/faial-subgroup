__device__ int *table[4];

__global__ void k(int cat) {
  int *row = table[cat];
  row[threadIdx.x] = threadIdx.x;
}
