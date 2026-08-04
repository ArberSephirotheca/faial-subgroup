// Two instantiations of one member template. Clang hangs both under the
// same function template, so taking only the first would keep the safe
// store and lose the racy one, and the kernel would clear.
struct Acc {
  int *a;
  float *b;
  template <typename T>
  __device__ void put(T *dst, int i, T v) { dst[i] = v; }
};

__global__ void k(Acc s) {
  s.put(s.a, threadIdx.x, (int)threadIdx.x);
  s.put(s.b, 0, (float)threadIdx.x);
}
