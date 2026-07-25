// Two loads of one array agree when their indices agree, the equality
// axiom that comes with modelling a read as an uninterpreted function.
// Under --block-dim=32 every thread's [t / 32] is 0, so both threads
// of a race witness load the same cell of A and share a base; the
// write index [base + t] then separates them by [t] alone and the
// kernel is race free. The two indices reach the solver as distinct
// terms, one per thread, so nothing but the equality axiom relates the
// two loads. Under --block-dim=64 the witness can straddle two warps,
// whose bases are unrelated, and one base can undercut the other by
// exactly the thread-id gap, so the write collides.
__global__ void k(int *A, int *out) {
  int t = threadIdx.x;
  int base = A[t / 32];
  out[base + t] = t;
}
