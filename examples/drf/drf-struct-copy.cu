// The same expansion where every thread copies its own element, so the
// leaf accesses stay disjoint and the kernel is race-free. Without this
// the expansion could report a race by spanning a dimension it should
// have kept per-thread.
struct Atom { double f[3]; };

__global__ void k(Atom *s, Atom *t) { s[threadIdx.x] = t[threadIdx.x]; }
