//saxpy,ind,ind
//
/*

Example: integer flows from array index

Here variable `j` is renamed to `_unknown_1`:
*/
__global__ void saxpy(int n, float a, float *x, float *y) {
   int i = blockIdx.x*blockDim.x + threadIdx.x;
   int j = x[i];
   if (i < n) y[j] = a*j;
}
