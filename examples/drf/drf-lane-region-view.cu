// A shared array of vectors read two ways: by lane, and as the flat run of
// scalars the lanes are interleaved in. Both name one region, so the two
// land on comparable cells; each thread stays on its own.
__global__ void k() {
  __shared__ float2 v[64];
  float *p = (float *)v;
  v[threadIdx.x].y = 1.0f;
  p[2 * threadIdx.x] = 2.0f;
}
