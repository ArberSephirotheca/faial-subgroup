// Barrier-crossing counterpart of the simpler loop example. The
// __syncthreads() splits the loop body into two phases and the racing write
// lands in the phase after the barrier. The in-source __assume(i ==
// threadIdx.x) is applied as a loop invariant to every phase, including the
// post-barrier one (the loop is aligned, so its first iteration is peeled;
// the invariant follows the peeling), pinning i to the thread's own
// threadIdx.x: a colliding pair would share a thread id, so the write is
// race-free. Dropping the __assume makes it racy.
__global__
void k(int n)
{
  __shared__ int A[256];
  for (int i = 0; i < n; i++) {
    __assume(i == threadIdx.x);
    __syncthreads();
    A[i] = threadIdx.x;
  }
}
