// The twin of racy-inherited-method.cu, indexing per thread. Without the
// call the kernel has no access at all and warns instead of clearing,
// so this one separates a resolved call from a lost one where its twin
// cannot.
struct Base { float *p; __device__ void put(int i, float v) { p[i] = v; } };
struct Derived : Base { };

__global__ void k(Derived a) { a.put(threadIdx.x, 1.0f); }
