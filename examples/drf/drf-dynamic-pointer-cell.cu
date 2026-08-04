// Each thread reads an address out of its own bucket, so which region the
// write lands in is decided by a value rather than statically. The
// address the read returns is the leading index of the write, and two
// cells of one table hold two addresses, so the threads stay apart.
// Naming the region instead needs one name to stand for every bucket's
// buffer at once, which is why this used to be discarded rather than
// answered.
struct Bucket { int *items; };

__global__ void k(Bucket *b) {
  b[threadIdx.x].items[0] = threadIdx.x;
}
