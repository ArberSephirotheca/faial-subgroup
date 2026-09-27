// The same binding with a per-thread element, so the member accesses
// through the pointer stay disjoint.
struct Atom { double f[3]; };

__global__ void k(Atom *s) {
  Atom *q = s;
  q[threadIdx.x].f[0] = 1.0;
}
