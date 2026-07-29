// The same shape as racy-2d-arg-offset.cu with the two writes in
// different rows. On its own this cannot catch a mis-scaled offset, since
// a write shifted out of row 1 is still one cell per thread, so it pins
// today's behaviour rather than guarding it.
__device__ void f(float y[][256]) { y[0][0] = 1.0f; }

__global__ void k(float x[][256]) {
  if (threadIdx.x == 0) f(x + 1);
  if (threadIdx.x == 1) x[0][0] = 2.0f;
}
