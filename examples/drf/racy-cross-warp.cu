// Cross-warp data race on shared memory. Two warps in the same block
// write into sdata[tid / 32] = sdata[0] / sdata[1]; thread 31 and
// thread 32 land in the same cell and belong to different warps, so
// the implicit pre-Volta same-warp barrier does not order them.
// --assume-warp-synch must NOT mask this race.
__global__ void k(float *out) {
  __shared__ float sdata[2];
  int tid = threadIdx.x;
  sdata[tid / 32] = tid;
  out[0] = sdata[0];
}
