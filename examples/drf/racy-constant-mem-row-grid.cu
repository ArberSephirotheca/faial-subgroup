__constant__ int *table[4];

__global__ void k(int *A, int cat) {
  A[blockIdx.x * blockDim.x + threadIdx.x] = threadIdx.x;
  int *row = table[cat];
  row[0] = threadIdx.x;
}
