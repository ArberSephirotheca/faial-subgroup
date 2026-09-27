__shared__ float y[1024];

__global__
void saxpy(float a, float *x)
{
  const unsigned long m = -1;
  y[((m >> 60) + 1) * threadIdx.x] = a*x[0];
}
