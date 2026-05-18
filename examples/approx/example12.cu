//saxpy,ind,ctrl
//@AccessState2
/*
Example 12: data flows from array to conditional, source array is rw.

Like example 2, but `y` is also written by the kernel, so the read
`y[i]` in the if-condition stays as the approx local `@AccessState1`.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (y[i]) x[i + 1] = a*x[i];
  y[i] = 0;
}
