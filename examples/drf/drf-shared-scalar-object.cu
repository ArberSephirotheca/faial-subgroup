// A scalar in shared memory is spelled with a subscript of its own, so
// [v] is written [v[0]] and that index reaches the object rather than a
// cell inside it. Counting it as a level off the type left the store
// naming an object whose lanes are the memory, and the store went missing
// while the lane read beside it resolved.
__global__ void k(double *out) {
  __shared__ double4 v;
  if (threadIdx.x == 0) v = make_double4(1, 2, 3, 4);
  __syncthreads();
  out[threadIdx.x] = v.x;
}
