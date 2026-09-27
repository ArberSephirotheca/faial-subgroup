// A method with template parameters of its own. Clang wraps such a
// declaration in a function template, and the record walk kept only
// plain methods and nested records, so a method like this was left out
// of the program: not declined, absent, and every access it makes with
// it.
struct Acc {
  int *out;
  template <typename T>
  __device__ void put(T v, int i) { out[i] = (int)v; }
};

__global__ void k(Acc a) { a.put(1.0f, 0); }
