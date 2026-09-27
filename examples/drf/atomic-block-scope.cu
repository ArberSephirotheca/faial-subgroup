// Block-scoped atomics serialise only against threads of the same
// block, so two threads of one block incrementing the same cell is DRF
// while two threads of different blocks are not. See the companion
// atomic-device-scope.cu, which differs only in the scope suffix and
// stays DRF at grid level. Needs --gridDim=2, since the default
// single-block grid leaves no second block to race with.
__global__ void k(int *counter) {
    atomicAdd_block(counter, 1);
}
