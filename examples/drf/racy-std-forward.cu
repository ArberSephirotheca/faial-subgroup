// [std::forward] and [std::move] are static casts written as calls. Only
// the pattern carries a body and a call binds to the instantiation, so
// resolving one finds nothing and the kernel is declined for a missing
// definition that is not missing.
#include <utility>

__global__ void k(int *A) {
  int *p = std::forward<int *>(A);
  p[0] = threadIdx.x;
}
