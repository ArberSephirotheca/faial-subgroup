// The twin of drf-lane-region-view.cu with the flat view one cell further
// on, so a thread writes the lane its neighbour writes by name. Naming the
// two views apart is what used to hide this.
__global__ void k() {
  __shared__ float2 v[64];
  float *p = (float *)v;
  v[threadIdx.x].y = 1.0f;
  p[2 * threadIdx.x + 3] = 2.0f;
}
