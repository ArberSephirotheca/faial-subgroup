__device__ void put(int *P, int i) {
  P[i] = i;
}

__global__ void k(int *A) {
  put(A + 1, threadIdx.x);
}
