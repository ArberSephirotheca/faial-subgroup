// Companion to drf-builtin-assume.cu with the assumption removed. With D
// unconstrained the prover may choose D = 1, so every thread writes out[0].
__global__
void k(int *out, int D)
{
  int tid = threadIdx.x;
  out[tid % D] = tid;
}
