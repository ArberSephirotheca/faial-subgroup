// A signed right shift rounds towards minus infinity, so for a negative [n]
// the value [n >> 1] is at most -1 and never 0: the guard is unsatisfiable
// and each thread writes its own cell. Rewriting the shift into a division
// that truncates towards zero answers 0 for n = -1 and opens the branch,
// where the threads collide on one cell.
//
// The [&] in the index is what makes this reach the bit-vector backend: the
// arithmetic encoder has no bitwise operators, so it gives up and the whole
// query is re-encoded over machine words, which is where a truncating
// division and a shift part ways.
__global__ void k(int *out, int n, int m) {
  int tid = threadIdx.x;
  if (n < 0 && (n >> 1) == 0) {
    out[m & 7] = tid;
  } else {
    out[tid] = 1;
  }
}
