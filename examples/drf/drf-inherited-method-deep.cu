// The base a method is found on need not be the immediate one, so the
// search walks the hierarchy rather than looking one level up.
struct Base { float *p; __device__ void put(int i, float v) { p[i] = v; } };
struct Middle : Base { };
struct Derived : Middle { };

__global__ void k(Derived a) { a.put(threadIdx.x, 1.0f); }
