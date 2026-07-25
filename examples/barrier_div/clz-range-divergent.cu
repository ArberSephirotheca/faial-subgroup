// The companion at a bound the declared range does not reach: two
// threads' results may straddle 16, so they genuinely disagree about
// reaching the barrier and the divergence stands.
__global__ void k(int *out) {
  int t = threadIdx.x;
  if (__clz(t) <= 16) {
    __syncthreads();
  }
  out[t] = t;
}
