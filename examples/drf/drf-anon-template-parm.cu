#include <type_traits>

// A template type parameter with no name, which is how a constraint is
// written in place: [class = typename enable_if<...>::type]. Only the
// value form of the idiom was tolerated, so the type form failed to parse
// and took the whole file with it.
struct S {
  template <typename U,
            class = typename std::enable_if<std::is_same<U, int>::value>::type>
  __device__ void put(int *out, U i) {
    out[i] = 1;
  }
};

__global__ void k(int *out) {
  S s;
  s.put(out, (int)threadIdx.x);
}
