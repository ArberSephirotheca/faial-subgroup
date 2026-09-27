//saxpy,ind,ind
//
/*
Example 11: integer flows from array index, source array is rw.

Like example 1, but the source array `x` is also written by the
kernel. The uniform-read rewrite fires on every array, not just
read-only ones, because any cross-thread write at the read's
address in the same barrier interval is already a race on its own
and so the extra read-equality cannot mask a race. So `j = x[i]`
is bound to the same `NCall($read_x, i)` shape as in example 1,
`j` is substituted away by `Encode_assigns`, and the per-kernel
data-dependence check reports `ind,ind`.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
   int i = blockIdx.x*blockDim.x + threadIdx.x;
   int j = x[i];
   if (i < n) y[j] = a*j;
   x[i] = 0;
}
