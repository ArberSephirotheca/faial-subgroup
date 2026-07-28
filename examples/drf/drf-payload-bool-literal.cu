__global__
void bool_literal_payload(bool *y)
{
  if (threadIdx.x == 0) y[0] = true;
  else if (threadIdx.x == 1) y[0] = true;
}
