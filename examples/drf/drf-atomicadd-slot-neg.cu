// Atomic-2 with a negative literal delta. The distinctness contract
// applies for any nonzero literal; the downstream slot writes are
// still race-free.
__global__ void k(int *counter, int *out) {
    int slot = atomicAdd(counter, -1);
    out[slot] = threadIdx.x;
}
