// A load inside a device function reaches the solver under the caller's
// array and index, so it agrees with a load the caller writes on that
// array itself. The call passes the interior pointer &A[5] and the callee
// reads p[i], which resolves to a load of A at 5 + t, the same cell the
// caller loads as A[5 + t]. The two loads agree, [u - v] is zero, and the
// write index is the thread id, so the kernel is race free. Keeping the
// callee's parameter as the array of the load, or losing the offset of the
// argument, leaves the two loads unrelated, and the free difference lets
// two threads collide on out. A is only read, so the only candidate race
// is on out.
__device__ int load(int *p, int i) { return p[i]; }

__global__ void k(int *A, int *out) {
  int t = threadIdx.x;
  int u = load(&A[5], t);
  int v = A[5 + t];
  out[t + u - v] = t;
}
