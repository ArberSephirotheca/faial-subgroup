// Every offset picked up along a chain of calls reaches the load. The
// kernel passes &A[5] to outer, which passes &p[3] to inner, so inner's
// q[i] is a load of A at 8 + t and agrees with the caller's own load of
// A[8 + t]. The two loads cancel in the write index, leaving the thread id,
// so the kernel is race free. Folding only the innermost offset, or none
// of them, separates the two loads and the free difference lets two
// threads collide on out. A is only read, so the only candidate race is
// on out.
__device__ int inner(int *q, int i) { return q[i]; }
__device__ int outer(int *p, int i) { return inner(&p[3], i); }

__global__ void k(int *A, int *out) {
  int t = threadIdx.x;
  int u = outer(&A[5], t);
  int v = A[8 + t];
  out[t + u - v] = t;
}
