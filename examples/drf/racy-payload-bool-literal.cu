__global__
void bool_literal_payload_racy(bool *y)
{
  if (threadIdx.x == 0) y[0] = true;
  else if (threadIdx.x == 1) y[0] = false;
}
