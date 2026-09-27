// The same reference-taken pointer beside one passed by value. The by-value
// array registers and keeps the kernel from reporting no accesses, so losing
// the array behind the reference reads as a race-free verdict rather than as
// a dropped access. Every thread writes q[0].
__global__ void k(float *p, float *q) {
  p[threadIdx.x] = 1;
  q[0] = threadIdx.x;
}

void byref(float *p, float *&q) { k<<<1, 32>>>(p, q); }
