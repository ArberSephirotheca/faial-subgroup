// An atomic on a scalar member of an array element, which is the shape a
// hash table built out of key-value cells uses. The address is a member
// path with an index for every subscript, and it has no spelling as a base
// plus one offset, so recovering it that way declined and the whole call
// was left as a call to atomicCAS, whose body faial cannot see. The kernel
// was then discarded for a missing function body.
struct Cell { unsigned key; unsigned val; };

__global__ void k(Cell *t) {
  if (threadIdx.x == 0) atomicCAS(&t[0].key, 0u, 7u);
  if (threadIdx.x == 1) t[0].key = 9u;
}
