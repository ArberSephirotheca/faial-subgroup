// The twin of racy-nested-pointer-field.cu, indexing per thread. Without
// the nested member the kernel has no access at all and warns instead of
// clearing, so this one separates a registered table from a dropped one
// where its twin cannot.
struct Tab  { int *d[2]; };
struct Nest { Tab inner; };

__global__ void k(Nest a) { a.inner.d[0][threadIdx.x] = threadIdx.x; }
