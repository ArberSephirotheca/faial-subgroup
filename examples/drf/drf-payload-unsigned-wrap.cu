__global__ void k(unsigned char *y) {
  if (threadIdx.x == 0) y[0] = 200;
  else if (threadIdx.x == 1) y[0] = 456;
}
