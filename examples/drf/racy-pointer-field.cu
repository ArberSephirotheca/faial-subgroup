struct V { int *p; };

__global__ void k(V v, int *B) {
  B[threadIdx.x] = 1;
  v.p[0] = threadIdx.x;
}
