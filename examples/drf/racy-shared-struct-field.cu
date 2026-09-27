struct Buf { int a[256]; };

__global__ void k() {
  __shared__ Buf s;
  s.a[0] = threadIdx.x;
}
