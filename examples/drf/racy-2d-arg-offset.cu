// A two-dimensional parameter passed at a non-zero offset. The offset is
// one row, and the callee's y[0][0] is therefore x[1][0], which the other
// thread writes directly. An offset carried in bytes with no step to
// convert it back would land the call-side write a row width away, the
// collision would vanish, and the kernel would come out data-race free.
__device__ void f(float y[][256]) { y[0][0] = 1.0f; }

__global__ void k(float x[][256]) {
  if (threadIdx.x == 0) f(x + 1);
  if (threadIdx.x == 1) x[1][0] = 2.0f;
}
