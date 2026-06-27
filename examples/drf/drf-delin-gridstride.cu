__global__ void k(float *a, int n) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;
  int stride = gridDim.x * blockDim.x;
  // Bound rounded up to a multiple of the stride => uniform trip count.
  int rounded = ((n - 1) / stride + 1) * stride;
  for (int i = idx; i < rounded; i += stride) {
    for (int j = 0; j < 4; j++) {
      int li = i + stride * j;
      if (li < n) a[li] = 1.0f;
    }
    __syncthreads();
  }
}
void run(float *a, int n) { k<<<512, 256>>>(a, n); }
