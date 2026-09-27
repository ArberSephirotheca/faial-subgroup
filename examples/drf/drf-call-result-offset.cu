// What comes back is a location with a displacement, not just an array.
// Thread 0 stores one cell along from thread 1's, so dropping the
// callee's offset puts both on cell zero and invents a race.
__device__ float *at(float *q, int i) { return q + i; }

__global__ void k(float *p) {
  if (threadIdx.x == 0) at(p, 1)[0] = 1.0f;
  if (threadIdx.x == 1) p[0] = 2.0f;
}
