// A power-of-two assumption doing real work. Masking with [n - 1]
// is the identity on [0, n) exactly when [n] is a power of two, so
// distinct threads keep distinct indices and the kernel is race
// free. The companion racy-pow2-mask.cu drops the assumption.
// Keeping [n] symbolic is what makes the assumption load-bearing:
// tying it to blockDim.x would resolve it to the default 1024,
// which is already a power of two.
__global__ void k(int n, int *y) {
  __requires(__is_pow2(n));
  int i = threadIdx.x;
  if (i < n) y[i & (n - 1)] = i;
}
