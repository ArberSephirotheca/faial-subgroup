__global__ void shuffle_does_not_order_memory(int *output) {
  __shared__ int scratch[1];
  if (threadIdx.x == 0) {
    scratch[0] = 1;
  }
  int value = __shfl_sync(0xffffffffu, (int)threadIdx.x, 0);
  output[threadIdx.x] = scratch[0] + value;
}
