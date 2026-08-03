// [T()] value-initialises a scalar, which is zero. Written out rather
// than left implicit, clang emits it as its own node, and an expression
// kind the parser does not know is a hard error rather than a decline.
template <typename T>
__device__ T zero() { return T(); }

__global__ void k(int *A) {
  A[zero<int>()] = threadIdx.x;
}
