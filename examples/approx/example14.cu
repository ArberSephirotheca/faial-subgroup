//saxpy,ind,ctrl
//j, k
/*
Example 14: data flows from array to upper bound of loop, source rw.

Like example 4, but `x` is also written by the kernel, so `j` and
the loop variable `k` (whose upper bound depends on `j`) are both
approx locals.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = 1; k < j; k++) {
    y[i + 1] = a * y[i];
  }
  x[i] = 0;
}
