// The same callable writing one element from every thread, beside an array
// the kernel touches directly. The direct write keeps the kernel from
// reporting no accesses, so losing the call reads as a race-free verdict
// rather than as a dropped access.
struct Store {
  int *out;
  __device__ void operator()(int i) const { out[0] = i; }
};

template <typename F>
__global__ void apply(F f, int *B) {
  B[threadIdx.x] = threadIdx.x;
  f(threadIdx.x);
}

void run(int *o, int *B) { apply<<<1, 32>>>(Store{o}, B); }
