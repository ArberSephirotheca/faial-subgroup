__global__
void bool_cond(int *out)
{
  bool b = threadIdx.x + 1;
  out[b] = threadIdx.x;
}
