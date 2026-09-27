// The twin of racy-atomic-dependent-callee.cu, with the store one cell
// along. Both are declined outright when the unresolved callee is not
// recognised, so this one separates a resolved atomic from a lost one
// where its twin cannot.
template <typename T>
__global__ void k(T *c) {
  if (threadIdx.x == 0) atomicAdd(c, 1);
  if (threadIdx.x == 1) c[1] = 1;
}
