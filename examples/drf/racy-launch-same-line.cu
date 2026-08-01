// Two launches of one kernel on one source line. A pseudo-kernel is named
// after its callee and its line, and that name is its whole identity, so the
// second launch overwrites the first in the kernel map. The 32-thread launch
// races on a[0]; the single-thread launch that shares its line cannot, and
// analysing only the second reports the file race-free.
__global__ void k(int *a) { a[0] = threadIdx.x; }

void run(int *d) { k<<<1, 32>>>(d); k<<<1, 1>>>(d); }
