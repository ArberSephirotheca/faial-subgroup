__global__
void neq_last(int *out, int n)
{
  if (threadIdx.x == 0) {
    for (int i = 0; i != n; i++) { out[i] = 1; }
  } else if (threadIdx.x == 1) {
    out[n - 1] = 2;
  }
}
