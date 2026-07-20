//saxpy,ind,ind
//
/*
Example 14: data flows from array to upper bound of loop, source rw.

Like example 4, but `x` is also written by the kernel. The uniform-
read rewrite binds `j = x[i]` to `NCall($read_x, i)` regardless of
the array being rw, so `j` is folded away by `Encode_assigns` and
the loop's upper bound becomes a function of `i`; the per-kernel
data-dependence check reports `ind,ind`.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = 1; k < j; k++) {
    y[i + 1] = a * y[i];
  }
  x[i] = 0;
}
