// The twin of declined-call-arity.cu with the argument's type matching the
// parameter's, so the lists line up and the call binds. Without it the pair
// above would pass whether the call bound or not.
struct T { int a[4]; };

__device__ void put(const T *t, int *out, int i) {
  out[i] = t[0].a[i];
}

__global__ void k(T *in, int *out) {
  put(in, out, threadIdx.x % 2);
}
