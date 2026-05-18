//saxpy,data,ind
//j
/*
Example 15: integer flows from array to index expression of a
multiplicative offset, source array is rw.

Like example 5, but `x` is also written, so `j = x[i]` stays as an
approx local.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
   int i = blockIdx.x*blockDim.x + threadIdx.x;
   int j = x[i];
   if (i < n) y[j * 2] = a * j;
   x[i] = 0;
}
