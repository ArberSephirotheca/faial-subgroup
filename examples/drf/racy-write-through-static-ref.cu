// A reference returned by a free function over a pointer parameter, which
// is how a matrix class spells element access without a receiver. The
// address is the parameter the caller bound to its own array, so the store
// lands on that array and meets the direct write below.
__device__ float &eltd(int i, float *d) { return d[i]; }

__global__ void k(float *g) {
  if (threadIdx.x == 0) eltd(1, g) = 1.0f;
  if (threadIdx.x == 1) g[1] = 2.0f;
}
