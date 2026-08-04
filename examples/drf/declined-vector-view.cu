// A view that reads a vector buffer as a flat array of scalars, so a cell
// of the view straddles the lanes the array was decomposed into. The
// launch site decomposes the argument too, which leaves the parameter
// bound to nothing at all: without the parameter counting as memory the
// write reads as a write to nowhere and is dropped, and the kernel answers
// for a program that never made it.
__global__ void k(float2 *a) {
  float *p = (float *)a;
  p[threadIdx.x] = 1.0f;
}

void run() {
  float2 *d;
  cudaMalloc((void **)&d, sizeof(float2) * 64);
  k<<<1, 64>>>(d);
}
