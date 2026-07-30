struct V { int *p; };

__global__ void k(V *s, int *B) {
  B[threadIdx.x] = 1;
  s->p[0] = threadIdx.x;
}
