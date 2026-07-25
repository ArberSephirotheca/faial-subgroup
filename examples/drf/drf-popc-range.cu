// __popc counts the set bits of a 32-bit value, so its result lies in
// 0..32 like __clz and __ffs. Same stride argument as
// drf-clz-range.cu, against a third entry carrying that range.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 64 + __popc(t)] = t;
}
