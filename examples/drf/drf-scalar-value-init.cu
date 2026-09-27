// The mirror of racy-scalar-value-init.cu, pinning the value rather than
// only its presence: thread 0 writes cell zero and every thread writes
// one cell above its own. The two meet only if [int()] is some index
// other than zero, which is what reading it as an unknown would allow.
template <typename T>
__device__ T zero() { return T(); }

__global__ void k(int *A) {
  if (threadIdx.x == 0) A[zero<int>()] = 1;
  A[threadIdx.x + 1] = 2;
}
