// A byte view of a two-dimensional array. A multi-dimensional array has
// no pointer step, so the view is left alone and p[3] stays a one-index
// access on an array whose other accesses carry two, printing as A[3]
// against A[0, 0]. Byte 3 does live in A[0][0], so the collision is real
// and the mixed arity hides it.
//
// The residual is the arity, not the scaling: there is no index the
// single-index access could take that would meet a two-index one.
__global__ void k() {
  __shared__ int A[4][4];
  char *p = (char *)A;
  if (threadIdx.x == 0) p[3] = 1;
  if (threadIdx.x == 1) A[0][0] = 7;
}
