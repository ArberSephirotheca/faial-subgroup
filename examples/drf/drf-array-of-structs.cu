// The same shape with the element index per thread, so the leading index
// keeps the writes apart.
struct Atom { double f[3]; };

__global__ void k(Atom *s) { s[threadIdx.x].f[0] = 1.0; }
