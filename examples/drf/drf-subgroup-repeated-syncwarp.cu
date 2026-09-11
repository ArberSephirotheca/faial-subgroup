__device__ int warp_reduce_sum(int value);

__global__ void repeated_syncwarp(int *output) {
  int value = threadIdx.x;

  for (int iteration = 0; iteration < 2; ++iteration) {
    value = warp_reduce_sum(value);
    __syncwarp();
  }

  output[threadIdx.x] = value;
}
