// The third 64-bit entry, sharing racy-clzll-range.cu as its
// companion.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 128 + __popcll(t)] = t;
}
