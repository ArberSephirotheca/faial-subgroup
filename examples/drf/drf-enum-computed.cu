enum ScaleFlag {
  ALIGN_CORNERS = (1 << 8),
  ANTIALIAS     = (1 << 9),
  NEXT          = ANTIALIAS + 1,
};

__global__
void saxpy(float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[i * NEXT + ALIGN_CORNERS] = a*x[i];
}
