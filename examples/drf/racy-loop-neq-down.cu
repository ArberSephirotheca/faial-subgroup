__global__
void neq_down(int *out, int n)
{
  for (int i = n; i != 0; i--) { out[i] = threadIdx.x; }
}
