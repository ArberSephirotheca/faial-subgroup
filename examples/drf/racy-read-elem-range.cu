// The companion of drf-read-elem-range.cu with an int element type,
// whose range spans far more than the stride, so the two writes do
// collide. The range must be the element type's and not a bound the
// analysis invents.
__global__ void k(int *out, int *idx) {
  int t = threadIdx.x;
  int d = idx[t];
  out[t * 256 + d] = t;
}
