// __clz is declared to return a number between 0 and 32, so the guard
// holds for every thread and no two threads can disagree about
// reaching the barrier. Without the declaration the result is an
// arbitrary integer, one thread's can exceed 32 while another's does
// not, and the barrier is reported divergent.
__global__ void k(int *out) {
  int t = threadIdx.x;
  if (__clz(t) <= 32) {
    __syncthreads();
  }
  out[t] = t;
}
