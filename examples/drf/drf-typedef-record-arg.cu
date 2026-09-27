// A typedef over a named struct is the alias every use spells, and a use
// may write a qualifier in front of it. While [const LatLong] resolved to
// nothing the plain name registered, the parameter stayed one region with
// no members while the launch argument was taken apart into its own, and
// the two sides of the binding named different memory.
typedef struct latLong { float lat; float lng; } LatLong;

__global__ void k(const LatLong *loc, float *d) {
  d[threadIdx.x] = loc[threadIdx.x].lat;
}

void run() {
  LatLong *p;
  float *d;
  cudaMalloc((void **)&p, sizeof(LatLong) * 64);
  cudaMalloc((void **)&d, sizeof(float) * 64);
  k<<<1, 64>>>(p, d);
}
