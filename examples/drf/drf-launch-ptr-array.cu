__global__
void ptr_array_arg_drf(int *out)
{
  out[threadIdx.x] = threadIdx.x;
}

void run()
{
  int *d[4];
  for (int i = 0; i < 4; i++)
    cudaMalloc(&d[i], 1024);
  ptr_array_arg_drf<<<1, 32>>>(d[1]);
}
