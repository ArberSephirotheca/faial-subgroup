__global__ void k(int *out, const float *f) {
  int t = threadIdx.x;
  out[t * 8 + __float2int_rz(f[t])] = t;
}
