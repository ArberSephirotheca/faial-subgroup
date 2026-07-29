namespace a { __device__ void f(int *A) { A[0] = threadIdx.x; } }
namespace b { __device__ void f(int *A) { A[threadIdx.x] = 1; } }

__global__ void k(int *A) {
  a::f(A);
}
