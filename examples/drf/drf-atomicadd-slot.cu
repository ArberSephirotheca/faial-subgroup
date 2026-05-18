// Atomic-2: atomicAdd unique-slot. The hardware guarantees that two
// threads atomic-adding the same counter cell with a nonzero literal
// delta see distinct return values, so the downstream slot writes
// from different threads cannot alias.
__global__ void k(int *counter, int *out) {
    int slot = atomicAdd(counter, 1);
    out[slot] = threadIdx.x;
}
