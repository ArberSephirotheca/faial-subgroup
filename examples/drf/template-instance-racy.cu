template <int N>
__device__ void f(int *A) {
  A[N] = threadIdx.x;
}

template <>
__device__ void f<1>(int *A) {
  A[threadIdx.x] = 1;
}

__global__ void k(int *A) {
  f<0>(A);
}
