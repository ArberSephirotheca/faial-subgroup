// Pre-Volta warp-synchronous halving reduction on a single warp.
// Without explicit __syncwarp() between halving steps this kernel
// has same-warp data races on sdata under post-Volta independent
// thread scheduling, and is DRF only under the lockstep semantics
// asserted by --assume-warp-synch.
__global__ void k(float *out) {
  __shared__ volatile float sdata[32];
  int tid = threadIdx.x;
  sdata[tid] = tid;
  if (tid < 16) sdata[tid] += sdata[tid + 16];
  if (tid <  8) sdata[tid] += sdata[tid +  8];
  if (tid <  4) sdata[tid] += sdata[tid +  4];
  if (tid <  2) sdata[tid] += sdata[tid +  2];
  if (tid <  1) sdata[tid] += sdata[tid +  1];
  if (tid == 0) out[0] = sdata[0];
}
