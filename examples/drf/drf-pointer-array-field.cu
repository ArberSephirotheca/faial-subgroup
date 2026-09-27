// The twin of racy-pointer-array-field.cu, indexing the row per thread.
// Without the member the kernel has no access at all and warns instead
// of clearing, so this one separates a registered table from a dropped
// one where its twin cannot.
struct Tab { int *d[2]; };

__global__ void k(Tab a) { a.d[0][threadIdx.x] = threadIdx.x; }
