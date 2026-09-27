// A kernel whose whole body is an invocation of a callable it takes by
// value, the shape used to write one elementwise kernel and supply the
// per-element body at each call site. The call carries the object as its
// leading argument and the callee reads its member through the object it
// was given, so the write lands on the caller's array. Each thread writes
// its own element.
struct Store {
  int *out;
  __device__ void operator()(int i) const { out[i] = i; }
};

template <typename F>
__global__ void apply(F f) { f(threadIdx.x); }

void run(int *o) { apply<<<1, 32>>>(Store{o}); }
