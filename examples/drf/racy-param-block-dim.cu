// Pinning a dimension with -p rather than --block-dim. The pin is
// substituted before the dimension defaults are inlined, so blockDim.x
// stops being a thread-global and the default must not be assigned over
// it.
//
// Racy with blockDim.x free, since the default block admits threads that
// are 32 apart. Pinned at 32 the remainder is the identity and each
// thread writes its own cell; pinned at 64 the collision is back.
__global__ void k(int *a) {
  a[threadIdx.x % 32] = threadIdx.x;
}
