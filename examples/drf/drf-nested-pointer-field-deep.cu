// The selection that reaches memory can be any length, so the walk goes
// down rather than one level in.
struct Tab   { int *d[2]; };
struct Mid   { Tab inner; };
struct Outer { Mid mid; };

__global__ void k(Outer a) { a.mid.inner.d[1][threadIdx.x] = threadIdx.x; }
