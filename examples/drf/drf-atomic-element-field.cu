// The same shape with every thread on its own element, and the plain write
// on a different member, so neither pair meets. Discarding the kernel would
// exit non-zero, so this half is what fails when the address is not
// recognised.
struct Cell { unsigned key; unsigned val; };

__global__ void k(Cell *t) {
  atomicCAS(&t[threadIdx.x].key, 0u, 7u);
  t[threadIdx.x].val = 9u;
}
