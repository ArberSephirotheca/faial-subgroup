// A flat view of a two-dimensional array counts cells, not rows, so the
// row is not the step to truncate by. Splitting the flat index across the
// axes puts p[5] at A[1][1] and p[6] at A[1][2], which are distinct.
// Taking one level of int[4][4] as the step instead would put both in row
// 1 and invent a collision between two distinct ints.
__global__ void k() {
  __shared__ int A[4][4];
  int *p = (int *)A;
  if (threadIdx.x == 0) p[5] = 1;
  if (threadIdx.x == 1) p[6] = 7;
}
