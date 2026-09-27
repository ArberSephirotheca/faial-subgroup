// The 64-bit intrinsics count over a wider word, so __clzll returns a
// number between 0 and 64 rather than between 0 and 32. Two threads
// collide when 128 * (t1 - t2) equals the difference of the two
// results, which a 0..64 result rules out.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 128 + __clzll(t)] = t;
}
