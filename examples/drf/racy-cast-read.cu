__global__ void k(int *out, const int *a) {
  int t = threadIdx.x;
  out[(char)a[t]] = t;
}
