// The companion of zero-accesses-no-memory.cu that does reach memory:
// each thread writes the cell its own index picks out, so the accesses
// are there and no two of them collide. The verdict stays drf, pinning
// that the zero-accesses check keys off the protocol being empty and
// not off the race proof being easy.
__global__ void k(int *out) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  out[i] = i;
}
