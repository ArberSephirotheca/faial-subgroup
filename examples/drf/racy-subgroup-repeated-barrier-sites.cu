__device__ int warp_reduce_sum(int value);

struct RepeatedBarrierParams {
  int iterations;
};

__global__ void repeated_barrier_sites(RepeatedBarrierParams params,
                                       int *output) {
  __shared__ int row_state[1];

  if (threadIdx.x == 0) {
    row_state[0] = 0;
  }
  __syncthreads();

  int value = 0;
  for (int iteration = 0; iteration < params.iterations; ++iteration) {
    int previous = row_state[0];
    value = warp_reduce_sum(previous + threadIdx.x);

    if (threadIdx.x == 0) {
      row_state[0] = value;
    }
    __syncthreads();
  }

  output[threadIdx.x] = value;
}
