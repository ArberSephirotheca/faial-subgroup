// The reinterpreted buffer starts part way in: [(const T *)(in + 4)] is
// four of the caller's cells forward, not four of the view's objects, and
// the callee's [t[0].a[i]] is [in[4 + i]]. The threads that read there meet
// the ones writing [in[i]], which they would not if the offset were scaled
// by the object rather than by the cell.
struct T { int a[4]; };

__device__ void put(const T *t, int *out, int i) {
  out[i] = t[0].a[i];
}

__global__ void k(int *in) {
  put((const T *)(in + 4), in, threadIdx.x % 8);
}
