// The twin of racy-std-forward.cu, indexing per thread. Both are
// declined outright when the call is not erased, so this one separates
// an erased cast from a lost one where its twin cannot.
#include <utility>

__global__ void k(int *A) {
  int *p = std::forward<int *>(A);
  p[threadIdx.x] = threadIdx.x;
}
