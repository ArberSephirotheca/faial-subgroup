__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  if (threadIdx.x <= 16 && n < 4) {
    y[10 + 2 * threadIdx.x] = a;
  } else {
    y[4 * threadIdx.x] = a;
  }
}
