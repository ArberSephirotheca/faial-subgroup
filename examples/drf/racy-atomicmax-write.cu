// An atomic with no cross-thread contract is still a memory access:
// atomicMax carries no guarantee about its returned value, but it
// conflicts with a plain write to the same cell. Thread A's atomicMax
// at threadIdx.x collides with thread B's write at threadIdx.x + 1.
__global__ void k(int *y) {
    atomicMax(&y[threadIdx.x], 1);
    y[threadIdx.x + 1] = 0;
}
