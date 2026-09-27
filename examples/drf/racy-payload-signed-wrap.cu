__global__ void k(char *y) {
  if (threadIdx.x == 0) y[0] = 200;
  else if (threadIdx.x == 1) y[0] = 100;
}
