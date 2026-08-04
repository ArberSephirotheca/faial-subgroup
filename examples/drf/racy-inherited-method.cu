// A method the receiver inherits rather than declares. The lookup keys
// on the record a call's receiver names, so a method declared on a base
// was no candidate for it and the call resolved to nothing: not a
// decline, the initialiser is dropped and the access with it.
struct Base { float *p; __device__ void put(int i, float v) { p[i] = v; } };
struct Derived : Base { };

__global__ void k(Derived a) { a.put(0, threadIdx.x); }
