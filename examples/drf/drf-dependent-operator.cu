// A comparison in a template that is never instantiated, so its operands
// have no types yet and clang leaves the overload unpicked. The only
// operator== with a body in the file compares pairs and has nothing to do
// with this call, so its write must not land in the kernel.
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
  if (a == in[1]) out[threadIdx.x] = a;
}
