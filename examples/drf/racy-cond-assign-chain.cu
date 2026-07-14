__global__ void chain(int *a, int n) {
  int i = threadIdx.x + blockIdx.x * blockDim.x;
  if (i < 1) i = i + n;
  if (i < 2) i = i + n;
  if (i < 3) i = i + n;
  if (i < 4) i = i + n;
  if (i < 5) i = i + n;
  if (i < 6) i = i + n;
  if (i < 7) i = i + n;
  if (i < 8) i = i + n;
  if (i < 9) i = i + n;
  if (i < 10) i = i + n;
  if (i < 11) i = i + n;
  if (i < 12) i = i + n;
  if (i < 13) i = i + n;
  if (i < 14) i = i + n;
  if (i < 15) i = i + n;
  if (i < 16) i = i + n;
  a[i] = threadIdx.x;
}
