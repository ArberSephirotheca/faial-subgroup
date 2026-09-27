// A byte view of a two-dimensional array. Retyping the byte address gives
// one flat element index where the array has two axes, so the flat index
// is split back across them: byte 3 is flat element 0, which is A[0][0],
// and it meets the direct subscript.
//
// Left flat, p[3] was a one-index access on an array whose other accesses
// carry two, printing as A[3] against A[0, 0], and no index the one-index
// access could take would meet a two-index one.
__global__ void k() {
  __shared__ int A[4][4];
  char *p = (char *)A;
  if (threadIdx.x == 0) p[3] = 1;
  if (threadIdx.x == 1) A[0][0] = 7;
}
