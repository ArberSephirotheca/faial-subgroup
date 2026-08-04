// A flat buffer read through a struct that reinterprets it. The parameter
// says the memory is an array of [T], the argument says it is an array of
// [int], and the member [a] has no array of its own in the caller: the
// access is where that member lands, [in[4 * g + i]]. Read as though the
// member kept a name, it named nothing and the whole callee was lost.
struct T { int a[4]; };

__device__ void put(const T *t, int *out, int i) {
  out[i] = t[0].a[i];
}

__global__ void k(int *in, int *out) {
  put((const T *)in, out, threadIdx.x);
}
