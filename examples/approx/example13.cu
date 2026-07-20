//saxpy,ind,ind
//
/*
Example 13: data flows from array to lower bound of loop, source rw.

Like example 3, but `x` is also written by the kernel. The uniform-
read rewrite binds `j = x[i]` to `NCall($read_x, i)` regardless of
the array being rw, so `j` is folded away by `Encode_assigns` and
the loop range becomes a function of `i`; the per-kernel
data-dependence check reports `ind,ind`.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = j; k < n; k++) {
    y[i] = y[i + 1];
  }
  x[i] = 0;
}
