//saxpy,ind,ctrl
//j, k
/*
Example 13: data flows from array to lower bound of loop, source rw.

Like example 3, but `x` is also written by the kernel, so the read
`j = x[i]` stays as an approx local, and the loop variable `k`
(whose range depends on `j`) is also approx.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = j; k < n; k++) {
    y[i] = y[i + 1];
  }
  x[i] = 0;
}
