// A store between two loads of one cell breaks their equality, so the
// two loads must not share a read symbol. Here the thread reads A[t],
// doubles it in place, and reads it back, making [b] twice [a] and the
// write index [t + a - b] equal to [t - a]. With A holding the
// identity every thread lands on out[0] and writes its own id, a race.
// Giving both loads the same symbol cancels [a - b] to zero, leaves
// the distinct index [t], and clears the race. No thread touches
// another thread's cell of A, so the only race is on out.
__global__ void k(int *A, int *out) {
  int t = threadIdx.x;
  int a = A[t];
  A[t] = a * 2;
  int b = A[t];
  out[t + a - b] = t;
}
