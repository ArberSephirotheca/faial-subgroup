struct V { int *p; };

__device__ void put(V v, int i) { v.p[i] = 1; }

__global__ void k(V v) {
  put(v, threadIdx.x);
}
