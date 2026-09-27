// Two calls of one device function on the same array at different offsets
// must stay apart. The arguments &A[5] and &A[9] resolve to loads of A at
// 5 + t and at 9 + t, two different cells, so [u - v] is unconstrained and
// two threads can land on the same cell of out. Dropping the offset of the
// argument gives both loads the index of the callee's parameter alone,
// which makes them agree, cancels [u - v], and leaves the distinct index
// [t], clearing the race. A is only read, so the only race is on out.
__device__ int load(int *p, int i) { return p[i]; }

__global__ void k(int *A, int *out) {
  int t = threadIdx.x;
  int u = load(&A[5], t);
  int v = load(&A[9], t);
  out[t + u - v] = t;
}
