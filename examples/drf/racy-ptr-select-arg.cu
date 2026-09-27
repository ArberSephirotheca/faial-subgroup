__device__ void touch(int *P, int v) { P[0] = v; }

__global__ void k(int *A, int *B, int c) {
  touch(c ? A : B, threadIdx.x);
}
