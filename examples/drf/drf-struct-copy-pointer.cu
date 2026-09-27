// The same copy and assignment with every thread on its own element, so
// the writes to the pointer storage stay disjoint.
struct Outer { float f; int *p; };

__global__ void k(Outer *s, Outer *t, int *A) {
  s[threadIdx.x] = t[threadIdx.x];
  s[threadIdx.x].p = A;
}
