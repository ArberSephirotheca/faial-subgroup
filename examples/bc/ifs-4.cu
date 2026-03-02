__shared__ float y[1024];
__device__ int f();
__global__
void saxpy(int n, float a, float *x)
{
  if (threadIdx.x <= 16 && x[0] < 4) {
    y[10 + 2 * threadIdx.x] = a;
  } else {
    y[4 * threadIdx.x] = a;
  }
}
