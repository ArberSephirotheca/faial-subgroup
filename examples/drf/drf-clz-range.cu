// __clz counts the leading zeros of a 32-bit value, so its result lies
// in 0..32. Nothing else relates the two threads' results, since the
// two calls are an uninterpreted function applied to distinct indices.
// Two threads collide when 64 * (t1 - t2) equals the difference of the
// two results, which the range rules out: that difference is at most 32.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 64 + __clz(t)] = t;
}
