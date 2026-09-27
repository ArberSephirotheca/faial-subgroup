// Copying a record copies the address a pointer member holds, so the copy
// touches that member's storage and meets a thread assigning it. The copy
// does not touch what the pointer points at, which is what copying a
// pointer means.
struct Outer { float f; int *p; };

__global__ void k(Outer *s, Outer *t, int *A) {
  if (threadIdx.x == 0) s[0] = t[0];
  if (threadIdx.x == 1) s[0].p = A;
}
