__global__ void k(int *dst, int n, int flag) {
  __assume(blockDim.y == 1 && blockDim.z == 1);
  __assume(gridDim.y == 1 && gridDim.z == 1);
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  int idx = i;
  if (flag) {
    idx = i + n;
  }
  dst[idx] = 0;
}
