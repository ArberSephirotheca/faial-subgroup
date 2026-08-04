// The counter hands out one index per thread, so the stores it directs
// do not collide. Its twin adds one unconditional store to cell zero,
// which the counter can also hand out.
__device__ int counter;

__global__ void k(int *A) {
  int i = atomicAdd(&counter, 1);
  A[i] = threadIdx.x;
}
