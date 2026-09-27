// A recursion the compiler has already unrolled: each instantiation calls
// the next one down, and the chain ends at an explicit specialization
// whose body is empty.
template <int N>
__device__ void step(int *d, int i) {
  d[i] = threadIdx.x;
  step<N - 1>(d, i + 1);
}

template <>
__device__ void step<0>(int *d, int i) {}

__global__ void k(int *d) {
  step<3>(d, threadIdx.x);
}
