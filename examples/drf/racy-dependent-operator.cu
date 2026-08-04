// The twin of drf-dependent-operator.cu with every thread on one output
// cell, so declining the unrelated operator does not take the kernel's own
// write with it.
__device__ int scratch[64];

namespace ns {
template <typename A, typename B>
struct pair { A first; B second; };

template <typename A, typename B>
__host__ __device__ inline bool operator==(pair<A, B> const &x,
                                           pair<A, B> const &y) {
  scratch[0] = threadIdx.x;
  return true;
}
}

template <typename KeyT>
__global__ void k(KeyT *out, const KeyT *in) {
  KeyT a = in[0];
  if (a == in[1]) out[0] = threadIdx.x;
}
