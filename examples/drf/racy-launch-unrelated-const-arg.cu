// The other half of drf-launch-impure-const-arg.cu: two host consts
// with unrelated opaque initialisers, one sizing the block and the
// other passed as the argument. They must not converge on one launch
// parameter, because nothing relates them.
//
// The race is what the collapse would hide. With the argument free of
// the block dimension, [threadIdx.x % m] repeats across threads for a
// small m, so two threads write the same cell.
int read_size();
int read_stride();

__global__ void k(int *a, int n) { a[threadIdx.x % n] = threadIdx.x; }

void run(int *d) {
  const int n = read_size();
  const int m = read_stride();
  k<<<1, n>>>(d, m);
}
