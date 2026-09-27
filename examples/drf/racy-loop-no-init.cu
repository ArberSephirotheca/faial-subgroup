__global__
void loop_no_init(int *out, int n)
{
  int i = threadIdx.x;
  int j = 0;
  for (; j != n; j++, i++)
    out[i] = threadIdx.x;
}
