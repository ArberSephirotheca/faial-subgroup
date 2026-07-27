__global__
void k(int *y, int n)
{
  y[n << -1] = threadIdx.x;
}
