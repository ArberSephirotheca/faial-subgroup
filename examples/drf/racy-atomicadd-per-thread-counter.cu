// Per-thread counter cells: each thread atomic-adds a different
// address. The distinct-return contract requires same-cell atomic
// ops, so distinctness cannot be inferred here and the downstream
// slot write is correctly flagged as racy.
__global__ void k(int *counter, int *out) {
    int slot = atomicAdd(&counter[threadIdx.x], 1);
    out[slot] = threadIdx.x;
}
