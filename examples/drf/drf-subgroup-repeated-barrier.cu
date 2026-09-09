__device__ int warp_reduce_sum(int value);

__global__ void repeated_barrier(int *output) {
  __shared__ int values[32];
  int value = threadIdx.x;

  for (int iteration = 0; iteration < 2; ++iteration) {
    values[threadIdx.x] = value;
    __syncthreads();
    value = warp_reduce_sum(values[threadIdx.x]);
    __syncthreads();
  }

  output[threadIdx.x] = value;
}
