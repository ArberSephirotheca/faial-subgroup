//saxpy,data,ind
//j
/*
Example 11: integer flows from array index, source array is rw.

Like example 1, but the source array `x` is also written by the
kernel, so the read-only / uniform-read rewrite does not apply.
`j = x[i]` stays as an approx local.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
   int i = blockIdx.x*blockDim.x + threadIdx.x;
   int j = x[i];
   if (i < n) y[j] = a*j;
   x[i] = 0;
}
