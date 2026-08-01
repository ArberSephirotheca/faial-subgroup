// A member function template of a class template, defined out of line and
// never instantiated. Its body stays dependent, so the unqualified call to
// the sibling member [same] reaches faial as an [UnresolvedMemberExpr]
// carrying neither a member name nor a base. That node must collapse to an
// unknown value: aborting on it loses the whole translation unit, including
// the kernel below, which writes to a single address from every thread and
// is racy.
template <typename T, int N>
struct Box {
  T v;
  template <int M> __host__ __device__ bool same(const Box<T, M> &o) const;
  template <int M> __host__ __device__ bool sameTwice(const Box<T, M> &o) const;
};

template <typename T, int N>
template <int M>
__host__ __device__ bool
Box<T, N>::sameTwice(const Box<T, M> &o) const {
  return same(o);
}

__global__ void k(int *a) { a[0] = threadIdx.x; }

void run(int *d) { k<<<1, 32>>>(d); }
