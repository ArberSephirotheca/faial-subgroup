__global__
void k(int T)
{
  __shared__ float buf[1024];
  __shared__ float buf2[1024];
  int base = threadIdx.x;
  for (int t = 0; t < T; t++) {
    buf[base] = 1.0f;
    base += blockDim.x;
    buf2[threadIdx.x] = 2.0f;
  }
}
