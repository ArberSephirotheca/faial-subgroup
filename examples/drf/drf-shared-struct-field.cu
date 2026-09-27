struct Two { int a[256]; int b[256]; };

__global__ void k(int *out) {
  __shared__ Two s;
  s.a[threadIdx.x] = 1;
  out[threadIdx.x] = s.b[threadIdx.x + 1];
}
