__global__ void k(char *out) {
  out[threadIdx.x] = 0;
}
