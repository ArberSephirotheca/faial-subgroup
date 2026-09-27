// The same store with every thread taking its own slot, so the writes to
// the pointer storage stay disjoint.
struct V { int *p; };

__global__ void k(V *s, int *A) {
  s[threadIdx.x].p = A;
}
