__global__
void decrement_loop(int n, int *out) {
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  while (auto i = n) {
    out[tid + i * blockDim.x * gridDim.x] = tid;
    i--;
  }
}
