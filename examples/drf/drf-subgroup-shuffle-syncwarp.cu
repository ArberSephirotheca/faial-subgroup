__global__ void syncwarp_orders_memory_after_shuffle(int *output) {
  __shared__ int scratch[1];
  if (threadIdx.x == 0) {
    scratch[0] = 1;
  }
  int value = __shfl_sync(0xffffffffu, (int)threadIdx.x, 0);
  __syncwarp();
  output[threadIdx.x] = scratch[0] + value;
}
