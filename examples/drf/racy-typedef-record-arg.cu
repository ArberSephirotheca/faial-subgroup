// The twin of drf-typedef-record-arg.cu with every thread on one output
// cell, so the alias resolving is not on its own enough to clear it.
typedef struct latLong { float lat; float lng; } LatLong;

__global__ void k(const LatLong *loc, float *d) {
  d[0] = loc[threadIdx.x].lat;
}

void run() {
  LatLong *p;
  float *d;
  cudaMalloc((void **)&p, sizeof(LatLong) * 64);
  cudaMalloc((void **)&d, sizeof(float) * 64);
  k<<<1, 64>>>(p, d);
}
