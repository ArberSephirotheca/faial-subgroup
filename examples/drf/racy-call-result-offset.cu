// The twin of drf-call-result-offset.cu, with the direct store moved
// onto the cell the displacement reaches.
__device__ float *at(float *q, int i) { return q + i; }

__global__ void k(float *p) {
  if (threadIdx.x == 0) at(p, 1)[0] = 1.0f;
  if (threadIdx.x == 1) p[1] = 2.0f;
}
