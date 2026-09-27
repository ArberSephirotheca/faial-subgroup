__global__
void bool_to_int(int *out)
{
  int f = !!threadIdx.x;
  out[f] = threadIdx.x;
}
