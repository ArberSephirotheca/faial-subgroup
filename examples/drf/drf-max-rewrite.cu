// The same body rung against [max]. Every thread id is non-negative,
// so max(t, 0) is t and each thread writes its own cell.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[max(t, 0)] = t;
}
