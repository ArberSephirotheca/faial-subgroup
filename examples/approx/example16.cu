//saxpy,data,ctrl
//j, k
/*
Example 16: data flows from array to RHS of a loop's write, source
array is rw.

Like example 6, but `x` is also written, so `j = x[i]` stays as an
approx local, and the loop variable `k` (whose range depends on
`j`) is also approx.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = j; k < n; k++) {
    y[k] = a * j;
  }
  x[i] = 0;
}
