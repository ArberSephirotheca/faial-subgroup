template <int N>
__device__ void f(int *A) {
  A[threadIdx.x + N] = 1;
}

template <>
__device__ void f<1>(int *A) {
  A[0] = threadIdx.x;
}

__global__ void k(int *A) {
  f<0>(A);
}
