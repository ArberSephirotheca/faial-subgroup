// __ffs returns the position of the lowest set bit plus one, or 0 when
// the argument is zero, so its result lies in 0..32 like __clz. Same
// stride argument as drf-clz-range.cu, against the second registry entry.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 64 + __ffs(t)] = t;
}
