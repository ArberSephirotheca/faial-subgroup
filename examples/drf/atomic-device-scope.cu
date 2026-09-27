// Device-scoped atomics never conflict with each other: the hardware
// serialises them across the whole device, so two threads in different
// blocks incrementing the same cell is DRF even at grid level. The
// companion atomic-block-scope.cu differs only in the scope suffix and
// is racy under the same flags. Both need --gridDim=2, since the
// default single-block grid leaves no second block to race with.
__global__ void k(int *counter) {
    atomicAdd(counter, 1);
}
