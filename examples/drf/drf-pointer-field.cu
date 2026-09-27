struct V { int *p; int n; };

__global__ void k(V v) {
  v.p[threadIdx.x + v.n] = 1;
}
