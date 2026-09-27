__global__
void bare_cond(int *out)
{
  for (int i = 4; i; i--)
    out[threadIdx.x * 8 + i] = threadIdx.x;
}
