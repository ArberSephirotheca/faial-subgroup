// The companion in the other direction: a signed right shift keeps the sign,
// so for a negative [n] the value [n >> 1] is still negative, the guard holds
// for any negative argument, and every thread writes out[0] with its own tid.
// Treating a signed shift as a zero-filling one would turn the shifted value
// positive and clear a race the kernel has.
__global__ void k(int *out, int n) {
  int tid = threadIdx.x;
  if (n < 0 && (n >> 1) < 0) {
    out[0] = tid;
  } else {
    out[tid] = 1;
  }
}
