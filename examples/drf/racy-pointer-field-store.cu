// Assigning a pointer member writes the storage that holds the address,
// which is memory of the object it sits in, so two threads storing into
// the same slot race. Before the storage had a name of its own the
// assignment became a read of the enclosing element and the store was
// lost.
struct V { int *p; };

__global__ void k(V *s, int *A) {
  s[threadIdx.x % 2].p = A;
}
