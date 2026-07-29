// A flat view of a two-dimensional array is not indexing in rows, so the
// row is not the step to truncate by. Taking one level of int[4][4] as
// the step would put p[5] and p[6] both in row 1 and invent a collision
// between two distinct ints, so a multi-dimensional array has no step at
// all and the view is left alone.
__global__ void k() {
  __shared__ int A[4][4];
  int *p = (int *)A;
  if (threadIdx.x == 0) p[5] = 1;
  if (threadIdx.x == 1) p[6] = 7;
}
