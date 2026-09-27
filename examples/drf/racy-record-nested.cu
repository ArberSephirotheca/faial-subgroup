// A record declared inside another record. The nested declaration is a
// child of the enclosing one, and descending only into methods dropped it,
// so [Outer::Inner] resolved to no fields and the kernel reported no
// accesses at all.
struct Outer { struct Inner { float g[4]; }; };

__global__ void k(Outer::Inner *z) {
  z->g[threadIdx.x % 4] = 2.0f;
}
