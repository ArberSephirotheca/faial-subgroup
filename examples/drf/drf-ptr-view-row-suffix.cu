// The same row view with the two writes on distinct cells of it: flat 5 is
// B[i][1][1] and flat 6 is B[i][1][2]. Splitting across every dimension
// rather than the suffix below the subscript would put them elsewhere and
// lose the distinction the row already fixes.
__global__ void k(int i) {
  __shared__ int B[2][4][4];
  int *row = (int *)B[i];
  if (threadIdx.x == 0) row[5] = 1;
  if (threadIdx.x == 1) row[6] = 7;
}
