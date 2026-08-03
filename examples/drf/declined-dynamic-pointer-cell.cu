// Each thread reads an address out of its own bucket, so which region the
// write lands in is decided by a value rather than statically. One name
// would have to stand for every bucket's buffer at once, and a name
// standing for a family is what the addressing model rules out, so the
// kernel is discarded rather than analysed under a merged name.
//
// Merged, all threads landed on one region at one index and the kernel
// reported a race that cannot happen unless two buckets share a buffer.
struct Bucket { int *items; };

__global__ void k(Bucket *b) {
  b[threadIdx.x].items[0] = threadIdx.x;
}
