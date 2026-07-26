__global__ void k(int *out) {
  int t = threadIdx.x;
  char a = t * 256;
  out[a] = t;
}
