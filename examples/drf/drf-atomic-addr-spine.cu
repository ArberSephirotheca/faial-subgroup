__global__ void k(int *p) {
  int a = 1, b = 2;
  atomicAdd(p + a + b, 1);
  if (threadIdx.x == 0) p[0] = 1;
}
