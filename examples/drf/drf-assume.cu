// Same kernel as drf-saxpy.cu, but with the in-source __assume() calls
// removed. Under --all-dims/--all-levels (which lets blockDim and gridDim
// range over 3D shapes), this is racy: two threads with the same
// threadIdx.x but different threadIdx.y or threadIdx.z compute the same
// `i` and race on y[i]. Inject the constraints with --assume from the
// command line, instead of writing __assume() in the source.
__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i] = a*x[i] + y[i];
}
