// The only launch of this kernel sits behind a condition that folds to false,
// so no run performs it. Minting a pseudo-kernel for it demotes the kernel and
// then prunes the wrapper's body against the false hypothesis, which reports a
// kernel with no accesses. A launch that cannot happen leaves the kernel where
// a kernel with no launch site would be: an entry point, checked with free
// dimensions, where each thread writes its own cell.
__global__ void k(int *a) { a[threadIdx.x] = 1; }

void run(int *d) { if (0 == 1) k<<<1, 32>>>(d); }
