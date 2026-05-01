//saxpy,data,ctrl
//@AccessState1, j1
/*
Example 10: Second loop variable is rendered as j1 and x[i] as unknonw_1.
*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  for (int j = 0; j < n; j++) {
    y[j] = i;
  }
  for (int j = x[i]; j < n; j++) {
    y[j] = i;
  }
}
