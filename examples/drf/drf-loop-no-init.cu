__global__
void loop_no_init_drf(int *out)
{
  int i = threadIdx.x * 8;
  int j = 0;
  for (; j != 8; j++, i++)
    out[i] = threadIdx.x;
}
