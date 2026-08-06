// A view that reads a vector buffer as a flat array of scalars, so a cell
// of the view straddles the lanes the array was decomposed into. Reaching
// the kernel through its launch site decomposes the argument as well, and
// the view has to survive that second decomposition rather than leave the
// write naming nothing. With the dimensions free, two threads of different
// rows share a column and collide; the launched configuration is one row
// wide, so each thread reaches a lane of its own.
__global__ void k(float2 *a) {
  float *p = (float *)a;
  p[threadIdx.x] = 1.0f;
}

void run() {
  float2 *d;
  cudaMalloc((void **)&d, sizeof(float2) * 64);
  k<<<1, 64>>>(d);
}
