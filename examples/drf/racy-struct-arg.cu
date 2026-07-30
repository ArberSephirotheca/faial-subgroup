struct V { int *p; };

__device__ void put(V v, int i) { v.p[0] = i; }

__global__ void k(V v, int *B) {
  B[threadIdx.x] = 1;
  put(v, threadIdx.x);
}
