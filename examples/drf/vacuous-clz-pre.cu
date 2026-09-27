// Each thread writes its own cell, so the kernel is data-race free on
// its own. Under the precondition [__clz(n) > 40] it is vacuously so:
// __clz is declared to return a number between 0 and 32, which the
// precondition contradicts, and every query under a contradictory
// precondition is unsatisfiable. --check-pre-sat reports that, and it
// can only see the contradiction if the declaration reaches the
// precondition query and not just the race query.
__global__ void k(int *out, int n) {
  int t = threadIdx.x;
  out[t] = n;
}
