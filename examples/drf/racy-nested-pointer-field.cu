// The table of pointers sits one object down. A member that is itself an
// object contributed nothing, so a use of [a.inner.d] named memory that
// was never registered and the store went missing.
struct Tab  { int *d[2]; };
struct Nest { Tab inner; };

__global__ void k(Nest a) { a.inner.d[0][0] = threadIdx.x; }
