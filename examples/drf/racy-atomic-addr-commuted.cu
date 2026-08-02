__global__ void k(int *p) {
  int i = 1;
  atomicAdd(i + p, 1);
  if (threadIdx.x == 0) p[1] = 1;
}
