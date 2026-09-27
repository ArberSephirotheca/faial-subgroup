// Two buckets hold two addresses, so slot zero of one buffer is not slot
// zero of the other. Each stored pointer is reached through a cell that is
// decided statically, so each names a region of its own: *b.items is the
// pointee of cell zero and *b.items[1] the pointee of cell one. Merged
// under one name they met, and the kernel reported a race that cannot
// happen.
struct Bucket { int *items; };

__global__ void k(Bucket *b) {
  if (threadIdx.x == 0) b[0].items[0] = 1;
  if (threadIdx.x == 1) b[1].items[0] = 2;
}
