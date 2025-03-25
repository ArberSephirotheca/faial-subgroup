__global__
void saxpy(int n, int k, float a, float *x, float *y)
{
  __requires (n - k > 0);
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float f = 0.0f;
  y[0] = 0;
  //for (uint offset = 0; offset + rowStrideA <= BM; offset += rowStrideA) {
  for (int j = 1; j + k < n; j++) {
    f += y[j];
    // y[0] ... y[n - 1]
  }
  // y[ 0 + n]
  y[i + n - k] = f;
}
