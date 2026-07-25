// A load carries the range of the array's element type. [idx] holds
// unsigned chars, so two threads' loaded values differ by at most 255
// and a stride of 256 keeps their writes apart. Without the element
// type reaching the solver the loaded value is an unbounded integer
// and the two writes collide.
__global__ void k(int *out, unsigned char *idx) {
  int t = threadIdx.x;
  int d = idx[t];
  out[t * 256 + d] = t;
}
