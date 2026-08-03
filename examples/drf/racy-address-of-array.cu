// The address of an array names the same cells the array does, so the
// store through [p] lands on [g] and meets the store written directly.
// Left unresolved, [p] is an ordinary local, the store through it is
// deleted, and the kernel comes out data-race free on the one store
// that remains.
typedef float grid_t[4][8];
__device__ grid_t g;

__global__ void k(int j) {
  grid_t *p = &g;
  if (threadIdx.x == 0) (*p)[j][0] = 1.0f;
  if (threadIdx.x == 1) g[j][0] = 2.0f;
}
