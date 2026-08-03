// A pointer member whose pointee is a record. The pointee is a region of
// its own, but nothing descends into it, so the members behind the pointer
// have no regions and the writes name nothing. Deleting them would leave a
// verdict about a different program, so the kernel is discarded.
//
// This is the shape a pointer to a scalar already handles: V { int *p; }
// gives *v.p directly, and only a record behind the pointer is missing.
struct In { double c[2]; };
struct Holder { In *b; };

__global__ void k(Holder *t) {
  t->b->c[threadIdx.x % 2] = 1.0;
}
