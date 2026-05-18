//saxpy,ind,ind
//
/*
Example 6: variables `j` and `k` come from an array and are used in a conditional.
Additionally, `k` is also used to index array `y`.

Approximate variables are `x[i]` and `x[i + 1]`.
*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  int k = x[i + 1];
  float sum = 0.0f;
  if (j < k) {
    y[k] = sum;
  }
}
