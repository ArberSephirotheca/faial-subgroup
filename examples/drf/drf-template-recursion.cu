// The twin of racy-template-recursion.cu with each instantiation a block
// further along, so the three writes a thread makes stay on cells of its
// own.
template <int N>
__device__ void step(int *d, int i) {
  d[i] = threadIdx.x;
  step<N - 1>(d, i + 1024);
}

template <>
__device__ void step<0>(int *d, int i) {}

__global__ void k(int *d) {
  step<3>(d, threadIdx.x);
}
