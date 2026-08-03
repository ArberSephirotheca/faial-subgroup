// An atomic on an array member of an array element, so the address carries
// two subscripts around a member selection. Recovered as a base plus one
// offset there is nothing to recover, since neither subscript is an
// addition.
struct Atom { double f[3]; };

__global__ void k(Atom *s) {
  if (threadIdx.x == 0) atomicAdd(&s[0].f[0], 1.0);
  if (threadIdx.x == 1) s[0].f[0] = 2.0;
}
