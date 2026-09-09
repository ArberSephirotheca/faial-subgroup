// __builtin_assume(cond) is clang's assumption builtin. Faial honours it
// as a precondition, the same way it honours the __assume() stub.
//
// Each thread writes out[tid % D]. With D unconstrained the prover picks a
// small D, so distinct threads can alias. The assumption tid < D forces
// tid % D == tid and makes the write race-free.
__global__
void k(int *out, int D)
{
  int tid = threadIdx.x;
  __builtin_assume(tid < D);
  out[tid % D] = tid;
}
