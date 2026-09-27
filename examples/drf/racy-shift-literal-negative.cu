// Both operands of the shift are literals, so the answer is settled while
// folding. [-2] is all ones but the last bit, and shifting it right one place
// arithmetically fills the vacated top bit with the sign, giving [-1]. The
// guard therefore holds and every thread writes out[0] with its own tid, so
// the writes disagree and the kernel races. Folding the shift as a logical
// one over a machine word answers with a large positive number instead, which
// closes the branch and hides the race.
__global__ void k(int *out) {
  int tid = threadIdx.x;
  if (((-2) >> 1) < 0) {
    out[0] = tid;
  } else {
    out[tid] = 1;
  }
}
