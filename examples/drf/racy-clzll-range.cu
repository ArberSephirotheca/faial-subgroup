// The companion at a stride of 64, which is exactly the width of the
// declared range: two results can differ by 64, so the kernel stays
// racy. It fails if __clzll inherits __clz's 0..32 range, which would
// clear a race the intrinsic genuinely has.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 64 + __clzll(t)] = t;
}
