// The companion: threads 0 and 1 both write out[0], since min(0, 3)
// is 0 and min(1, 3) is 1. The body has to be min's own graph for
// this to stay racy, because rewriting the call to its second
// argument, to a constant, or to max makes every index distinct.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t - min(t, 3)] = t;
}
