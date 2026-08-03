// The mirror of racy-address-of-array.cu, with the direct store moved
// one cell along. The store through [p] still lands on [g], and it
// lands on the cell the subscripts name rather than on the array's
// first.
typedef float grid_t[4][8];
__device__ grid_t g;

__global__ void k(int j) {
  grid_t *p = &g;
  if (threadIdx.x == 0) (*p)[j][0] = 1.0f;
  if (threadIdx.x == 1) g[j][1] = 2.0f;
}
