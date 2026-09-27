// An unsigned right shift fills the vacated bits with zero. At n = 0 the
// value [m] underflows to every bit set, and shifting that right by anything
// from 1 to 63 clears at least the top bit, so [m >> s] is below the all-ones
// value and [(m >> s) + 1] is never 0: the guard is unsatisfiable and each
// thread writes its own cell. Carrying the shift to the solver as a signed
// one fills with the sign instead, leaves every bit set for any [s], and
// opens the branch onto a single cell.
__global__ void k(int *out, unsigned long n, unsigned int s) {
  int tid = threadIdx.x;
  unsigned long m = n - 1;
  if (n == 0 && s >= 1 && s < 64 && (m >> s) + 1 == 0) {
    out[0] = tid;
  } else {
    out[tid] = 1;
  }
}
