// Two atomics of the same scope never conflict with each other at block
// level, whatever the operation. atomicExch has no cross-thread
// contract, so the returned value is unconstrained, but the accesses
// themselves are serialised and the kernel is DRF.
__global__ void k(int *y) {
    atomicExch(y, threadIdx.x);
}
