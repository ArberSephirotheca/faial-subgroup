//saxpy,ind,ind
//
/*
Example 12: data flows from array to conditional, source array is rw.

Like example 2, but `y` is also written by the kernel. The uniform-
read rewrite fires on every array, so the read `y[i]` in the if-
condition is bound to `NCall($read_y, i)` and folded away by
`Encode_assigns`; the conditional and the writes are functions of
`i`, so the per-kernel data-dependence check reports `ind,ind`.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (y[i]) x[i + 1] = a*x[i];
  y[i] = 0;
}
