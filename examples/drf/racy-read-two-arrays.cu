// One device function inlined against two arrays keeps the arrays apart.
// Both calls pass offset 5 and the same index, so the two loads differ
// only in the array they read, A against B, and their values are
// unrelated. The difference [u - v] is then unconstrained and two threads
// can land on the same cell of out. A read symbol shared by every array,
// or one named after the callee's parameter, makes the two loads
// identical, cancels [u - v], and leaves the distinct index [t], clearing
// the race. A and B are only read, so the only race is on out.
__device__ int load(int *p, int i) { return p[i]; }

__global__ void k(int *A, int *B, int *out) {
  int t = threadIdx.x;
  int u = load(&A[5], t);
  int v = load(&B[5], t);
  out[t + u - v] = t;
}
