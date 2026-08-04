// Storing through the dereference of what a call returns. The pointer is
// bound to a name of its own and the store is that pointer's first cell,
// the same as subscripting it by zero, which was already handled. Spelled
// with a star the store went missing, and nothing reported that it had.
__device__ float *ptr(float *d, int i) { return d + i; }

__global__ void k(float *g) {
  *ptr(g, threadIdx.x % 4) = 1.0f;
}
