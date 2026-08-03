// A pointer member is two regions: the storage holding the address, named
// s.p, and what it points at, named *s.p. They occupy different bytes, so
// a thread writing the address it stores cannot collide with a thread
// writing through the address, whatever the indices.
struct V { int *p; };

__global__ void k(V *s) {
  s[threadIdx.x].p = 0;
  s->p[0] = 1;
}
