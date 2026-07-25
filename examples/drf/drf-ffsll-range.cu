// The second 64-bit entry. Its racy companion is
// racy-clzll-range.cu, which pins that 0..64 is not narrowed.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 128 + __ffsll(t)] = t;
}
