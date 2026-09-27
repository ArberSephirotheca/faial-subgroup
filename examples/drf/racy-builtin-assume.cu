// Companion to drf-builtin-assume.cu with the __builtin_assume removed. With
// D unconstrained the prover picks D = 1 and two threads both write out[0],
// so the kernel races. This pins that the companion's DRF verdict is
// genuinely due to honouring __builtin_assume, not a vacuous pass.
__global__
void k(int *out, int D)
{
  int tid = threadIdx.x;
  out[tid % D] = tid;
}
