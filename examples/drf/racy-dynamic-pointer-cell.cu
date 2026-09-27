// The twin of drf-dynamic-pointer-cell.cu, where two threads reach one
// bucket. They read the same cell, so they hold the same address and the
// same offset, and they meet. Being apart in the pair above is a fact
// about which cell each thread reads, not about the addressing.
struct Bucket { int *items; };

__global__ void k(Bucket *b) {
  b[threadIdx.x % 2].items[0] = threadIdx.x;
}
