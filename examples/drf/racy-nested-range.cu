// The companion at a stride of 64, the width of the two ranges
// summed, so the two writes do collide.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 64 + __ffs(t) + __clz(__ffs(t))] = t;
}
