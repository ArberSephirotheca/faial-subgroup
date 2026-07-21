// __builtin_assume(cond) is clang's assumption builtin. Faial honours it
// as a precondition, the same way it honours the __assume() stub.
//
// Each thread writes out[tid % D]. With D unconstrained the prover picks a
// small D (e.g. D = 1, every thread writes out[0]) so two threads with
// distinct tid share tid % D and the write races. The assumption tid < D
// forces tid % D == tid, so every thread writes a distinct cell and the
// kernel is race-free. Dropping the assume makes it racy
// (racy-builtin-assume.cu).
__global__
void k(int *out, int D)
{
  int tid = threadIdx.x;
  __builtin_assume(tid < D);
  out[tid % D] = tid;
}
