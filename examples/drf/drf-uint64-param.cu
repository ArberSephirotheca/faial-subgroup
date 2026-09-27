// The companion below the range. [n + 1 == 0] needs [n] to be negative,
// which the unsigned domain rules out, so the racy branch is unreachable.
// An unsigned 64-bit type has no writable upper end but its lower end is
// zero, and dropping that end along with the other turns this racy.
__global__ void uint64_param(int *out, unsigned long n) {
  int tid = threadIdx.x;
  if (n + 1 == 0) {
    out[0] = tid;
  } else {
    out[tid] = 1;
  }
}
