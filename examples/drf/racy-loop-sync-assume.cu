// Baseline for drf-loop-sync-assume.cu. The __syncthreads() splits the
// loop body into two phases; the racing write sits in the phase after the
// barrier. Every thread writes A[i] with its own threadIdx.x, so two
// threads collide on the same cell with different values: a data race.
__global__
void k(int n)
{
  __shared__ int A[256];
  for (int i = 0; i < n; i++) {
    __syncthreads();
    A[i] = threadIdx.x;
  }
}
