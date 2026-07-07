__device__ float qfn(int) { return 0; }

template<float (*F)(int)> __device__ float call() { return F(0); }

__global__ void k(float * d)
{
  d[threadIdx.x] = call<qfn>();
}
