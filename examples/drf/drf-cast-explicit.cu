__global__ void k(int *out) {
  int t = threadIdx.x;
  out[(char)(t * 256)] = t;
}
