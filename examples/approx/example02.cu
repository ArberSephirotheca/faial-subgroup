//saxpy,ind,ctrl
//@AccessState1
/*
Example 2: data flows from array to conditional

Variable y[i] is _unknown_1 and x[i] is _unknown_2 both approximate.
*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (y[i]) x[i + 1] = a*x[i];
}
