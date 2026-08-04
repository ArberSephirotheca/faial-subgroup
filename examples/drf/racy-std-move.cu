// The same for [std::move], which is the other cast of the pair.
#include <utility>

__global__ void k(int *A) {
  int *p = std::move(A);
  p[0] = threadIdx.x;
}
