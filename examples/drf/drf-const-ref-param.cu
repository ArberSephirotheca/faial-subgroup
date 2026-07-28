__device__ void put(int *A, const int &i) {
  A[i] = i;
}

__global__ void k(int *A) {
  int i = threadIdx.x;
  put(A, i);
}
