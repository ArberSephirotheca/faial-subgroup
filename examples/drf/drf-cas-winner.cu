// Atomic-3: atomicCAS winner-uniqueness. The hardware guarantees at
// most one thread per address sees [old == SENTINEL], so the write
// to [values[loc]] guarded by [if (old == -1)] cannot race even
// when [loc] would otherwise admit collisions (here [loc] comes from
// an indirect lookup so two distinct threads may map to the same
// slot).
__global__ void k(int n, int *keys, int *values, int *hashes) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    int loc = hashes[idx] % n;
    int old = atomicCAS(&keys[loc], -1, idx);
    if (old == -1) {
        values[loc] = idx;
    }
}
