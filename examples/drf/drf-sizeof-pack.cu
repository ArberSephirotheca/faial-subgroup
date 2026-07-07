template<class... Ts> __device__ int npack() { return sizeof...(Ts); }

__global__ void k(int * d)
{
  d[threadIdx.x] = npack<int, float>();
}
