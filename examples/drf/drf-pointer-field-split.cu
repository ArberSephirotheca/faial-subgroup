// A pointer member is two regions: the storage holding the address, named
// s.p, and what it points at, named *s.p. They occupy different bytes, so
// a thread writing the address it stores cannot collide with a thread
// writing through the address, whatever the indices.
//
// Following the address means reading it, so the store is kept off the
// cell that [s->p] reads. Writing that cell would be a race about the
// storage rather than about the split this is here to show.
struct V { int *p; };

__global__ void k(V *s) {
  s[threadIdx.x + 1].p = 0;
  s->p[0] = 1;
}
