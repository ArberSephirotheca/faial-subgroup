// The __assume() sits inside the loop and mentions the loop counter i,
// so it is routed onto the loop as an invariant (surviving phase
// splitting) rather than a one-shot in-body guard. It pins i to the
// thread's own threadIdx.x, so a colliding pair (same i) would have to
// share a thread id: the write is race-free. Dropping the __assume makes
// it racy (racy-loop-assume.cu).
__global__
void k(int *A, int n)
{
  for (int i = 0; i < n; i++) {
    __assume(i == threadIdx.x);
    A[i] = threadIdx.x;
  }
}
