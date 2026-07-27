// The race is reachable only for an argument beyond the 32-bit signed
// range. [n] is a [long], whose ends no OCaml int can name, so the only
// sound hypothesis about it is none; bounding it by the 32-bit signed
// range instead makes the racy branch unreachable and reports data-race
// freedom on a kernel that races.
#define BEYOND_INT32 3000000000L

__global__ void int64_param(int *out, long n) {
  int tid = threadIdx.x;
  if (n > BEYOND_INT32) {
    out[0] = tid;
  } else {
    out[tid] = 1;
  }
}
