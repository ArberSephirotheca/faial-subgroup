struct W {
  __device__ static void put(int *A, int i) { A[0] = i; }
};

__global__ void k(int *A, int *B) {
  B[threadIdx.x] = 1;
  W::put(A, threadIdx.x);
}
