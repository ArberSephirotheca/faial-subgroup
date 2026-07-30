struct W {
  __device__ static void put(int *A, int i) { A[i] = i; }
};

__global__ void k(int *A) {
  W::put(A, threadIdx.x);
}
