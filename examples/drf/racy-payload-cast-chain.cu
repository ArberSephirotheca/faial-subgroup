__global__ void k(unsigned short *y) {
  if (threadIdx.x == 0) y[0] = (char)200;
  else if (threadIdx.x == 1) y[0] = 200;
}
