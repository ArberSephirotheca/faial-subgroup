// The twin of racy-member-template.cu, indexing per thread. Without the
// method the kernel has no access at all and warns instead of clearing,
// so this one separates a parsed method from a missing one where its
// twin cannot.
struct Acc {
  int *out;
  template <typename T>
  __device__ void put(T v, int i) { out[i] = (int)v; }
};

__global__ void k(Acc a) { a.put(1.0f, threadIdx.x); }
