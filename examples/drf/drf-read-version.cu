// A store between two loads separates them without making either one
// thread-local. Every thread loads A[0], one thread overwrites it
// between two barriers, and every thread loads it again. The two loads
// must not be equated, since the second returns 7 and the first does
// not, but each is still the same value in every thread, so the write
// index [t + a - b] offsets a shared constant by the thread id and the
// kernel is race free. Modelling the second load as an unknown local
// instead loses the agreement across threads and reports a race.
__global__ void k(int *A, int *out) {
  int t = threadIdx.x;
  int a = A[0];
  __syncthreads();
  if (t == 0) { A[0] = 7; }
  __syncthreads();
  int b = A[0];
  out[t + a - b] = t;
}
