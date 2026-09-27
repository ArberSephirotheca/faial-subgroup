// The companion: threads 0 through 4 all write out[4]. Rewriting the
// call to its first argument would make the writes distinct, so this
// pins that the body is max's graph and not the identity.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[max(t, 4)] = t;
}
