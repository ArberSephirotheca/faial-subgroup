//saxpy,ind,ind
//
/*
Example 15: integer flows from array to index expression of a
multiplicative offset, source array is rw.

Like example 5, but `x` is also written. The uniform-read rewrite
binds `j = x[i]` to `NCall($read_x, i)` regardless of the array
being rw, so `j` is folded away by `Encode_assigns` and the index
`j * 2` becomes a function of `i`; the per-kernel data-dependence
check reports `ind,ind`.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
   int i = blockIdx.x*blockDim.x + threadIdx.x;
   int j = x[i];
   if (i < n) y[j * 2] = a * j;
   x[i] = 0;
}
