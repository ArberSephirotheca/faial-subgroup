//saxpy,ind,ind
//
/*

Example 5: Index `j` is read from an array and then used to access array `y`.

The only approximate variable is `x[i]`.

*/
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  y[j] = a*j;
}
