struct Buf { int a[256]; };

__global__ void k(Buf b, int *B) {
  B[threadIdx.x] = 1;
  b.a[0] = threadIdx.x;
}
