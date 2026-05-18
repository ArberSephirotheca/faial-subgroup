//saxpy,ind,ind
//
/*
Example 4: data flows from array to upper bound of loop

Approximate variables are `x[i]` (which is `j`), then `k` depends on `x[i]`.

 */
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i]; // upper bound comes from array x
  for (int k = 1; k < j; k++) {
    y[i + 1] = a * y[i];
  }
}
