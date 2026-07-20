//saxpy,ind,ind
//
/*
Example 16: data flows from array to RHS of a loop's write, source
array is rw.

Like example 6, but `x` is also written. The uniform-read rewrite
binds `j = x[i]` to `NCall($read_x, i)` regardless of the array
being rw, so `j` is folded away by `Encode_assigns` and both the
loop range and the write RHS become functions of `i`; the
per-kernel data-dependence check reports `ind,ind`.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = j; k < n; k++) {
    y[k] = a * j;
  }
  x[i] = 0;
}
