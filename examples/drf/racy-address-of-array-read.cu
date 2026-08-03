// The read side of racy-address-of-array.cu: the subscripts follow the
// dereference rather than being applied to what a cell of [p] holds, so
// the load reaches [g] with all its indices and collides with the store.
typedef float grid_t[4][8];
__device__ grid_t g;

__global__ void k(int j, float *out) {
  grid_t *p = &g;
  if (threadIdx.x == 0) out[0] = (*p)[j][0];
  if (threadIdx.x == 1) g[j][0] = 1.0f;
}
