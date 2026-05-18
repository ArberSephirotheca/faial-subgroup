//saxpy,ind,ind
//
/*
Example 3: data flows from array to lower bound of loop

Variable `x[i]` is `j`, `k` depends on j.

*/
__global__ void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i]; // lower bound comes from array x
  for (int k = j; k < n; k++) {
    y[i] = y[i + 1];
  }
}
