// Baseline for drf-loop-assume.cu. Every thread writes A[i] on every
// iteration, storing its own threadIdx.x, so two threads collide on the
// same cell with different values: a data race. Adding the in-source
// __assume(i == threadIdx.x) that drf-loop-assume.cu carries removes it.
__global__
void k(int *A, int n)
{
  for (int i = 0; i < n; i++)
    A[i] = threadIdx.x;
}
