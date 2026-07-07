__device__ void report(const char * s) { }

__global__
void k(int * d)
{
  report(__FUNCTION__);
  report(__func__);
  report(__PRETTY_FUNCTION__);
  d[threadIdx.x] = 0;
}
