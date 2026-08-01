// A by-value struct argument passed to a [const] parameter. The typedef is
// part of the case: it makes clang write the parameter as [const V] and
// desugar it separately, which is what leaves the qualifier sitting on the
// name a record lookup keys on. A record is registered under its tag alone,
// so the qualifier has to come off, or the parameter stays opaque and [v.p]
// inside [put] binds to nothing, leaving the kernel with no accesses. The
// caller names its struct [w] so that the callee's members cannot resolve
// by sharing a name with the caller's.
typedef struct V { int *p; } V;

__device__ void put(const V v, int i) { v.p[i] = 1; }

__global__ void k(V w) { put(w, threadIdx.x); }
