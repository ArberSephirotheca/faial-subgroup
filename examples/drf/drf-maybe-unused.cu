__global__ void k(int * d)
{
  [[maybe_unused]] const void * p = nullptr;
  d[threadIdx.x] = 0;
}
