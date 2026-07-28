// A kernel whose whole body stays in registers: the arithmetic never
// reaches an array, so the protocol holds no memory access at all. With
// no pair of accesses to compare, every race query is trivially
// discharged and the kernel would read as data-race free. The verdict
// is zero-accesses instead, because an access-free protocol is a sign
// that the accesses were dropped while inferring the protocol rather
// than a sign that the kernel is safe.
__global__ void k(int n) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  int x = i * n;
  x = x + i;
}
