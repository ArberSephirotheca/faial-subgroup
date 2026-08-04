// A call whose argument is not a name, so there is nothing to expand into
// the parameter's members and the two lists cannot be lined up. The call
// used to be left in place, which [Encode_assigns] drops without trace, so
// the whole callee vanished and the kernel reported no accesses at all
// rather than saying what it could not do.
struct T { int a[4]; };

__device__ void put(const T *t, int *out, int i) {
  out[i] = t[0].a[i];
}

__global__ void k(int *in, int *out) {
  put((const T *)(in + 4), out, threadIdx.x);
}
