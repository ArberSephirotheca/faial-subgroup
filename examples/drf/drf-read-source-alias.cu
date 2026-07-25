// A pointer bound to the interior of an array is resolved to that array
// before any load is given its read symbol, so a load through the pointer
// agrees with a load written directly on the array. Here [p] names A at
// offset 5, so p[t] and A[5 + t] are the same cell, [u - v] is zero, and
// each thread writes its own cell of out. Naming the load after the
// pointer instead of after A leaves the two loads unrelated and the free
// difference lets two threads collide. A is only read, so the only
// candidate race is on out.
__global__ void k(int *A, int *out) {
  int t = threadIdx.x;
  int *p = &A[5];
  int u = p[t];
  int v = A[5 + t];
  out[t + u - v] = t;
}
