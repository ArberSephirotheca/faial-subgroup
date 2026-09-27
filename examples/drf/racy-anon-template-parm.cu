#include <type_traits>

// The twin of drf-anon-template-parm.cu with two threads on one cell, so
// the method's access has to arrive for the race to be found.
struct S {
  template <typename U,
            class = typename std::enable_if<std::is_same<U, int>::value>::type>
  __device__ void put(int *out, U i) {
    out[i] = i;
  }
};

__global__ void k(int *out) {
  S s;
  s.put(out, (int)(threadIdx.x % 2));
}
