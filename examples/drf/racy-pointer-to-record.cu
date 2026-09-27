// A pointer member whose pointee is a record. Its members are regions of
// their own, named through the crossing as [t.b->c], so the write lands
// somewhere nameable and two threads sharing a cell meet. While nothing
// descended past the pointer these writes named nothing, and the kernel
// was discarded rather than answered.
struct In { double c[2]; };
struct Holder { In *b; };

__global__ void k(Holder *t) {
  t->b->c[threadIdx.x % 2] = threadIdx.x;
}
