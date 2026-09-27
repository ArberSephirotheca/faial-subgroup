// The twin of drf-shared-scalar-object.cu with the barrier taken out, so
// the lane one thread stores is the lane the others read.
__global__ void k(double *out) {
  __shared__ double4 v;
  if (threadIdx.x == 0) v = make_double4(1, 2, 3, 4);
  out[threadIdx.x] = v.x;
}
