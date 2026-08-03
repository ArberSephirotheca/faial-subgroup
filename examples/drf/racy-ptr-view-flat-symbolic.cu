// A flat index the front end cannot fold, so the split has to stand as
// arithmetic rather than collapse to literals: the two axes come out as a
// division and an unsigned modulus of the whole expression. Threads 0 and
// 2 reach the same flat element and race.
__global__ void k(int base) {
  __shared__ int A[4][4];
  int *p = (int *)A;
  p[base + threadIdx.x % 2] = threadIdx.x;
}
