// Companion to drf-pow2-mask.cu without the assumption. With [n]
// free, masking by [n - 1] collides: at n = 6 both i = 1 and i = 3
// mask to 1, so two threads write one cell.
__global__ void k(int n, int *y) {
  int i = threadIdx.x;
  if (i < n) y[i & (n - 1)] = i;
}
