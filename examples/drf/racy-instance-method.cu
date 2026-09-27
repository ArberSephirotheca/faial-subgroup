__device__ int g[256];

struct W {
  __device__ void put(int i) { g[0] = i; }
};

__global__ void k() {
  W w;
  w.put(threadIdx.x);
}
