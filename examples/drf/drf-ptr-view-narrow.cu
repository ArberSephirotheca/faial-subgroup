// The other half of racy-ptr-view-narrow.cu: byte 5 lives in element 1
// and the direct write is to element 0, so the narrowing must not report
// a collision. Truncating the byte index to its element is exact, not an
// over-approximation that lumps neighbouring bytes together.
__global__ void k(int *A) {
  char *p = (char *)A;
  if (threadIdx.x == 0) p[5] = 1;
  if (threadIdx.x == 1) A[0] = 7;
}
