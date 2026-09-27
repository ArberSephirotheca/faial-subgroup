struct Buf { int a[256]; };

__device__ Buf g;

__global__ void k(int *B) {
  B[threadIdx.x] = 1;
  g.a[0] = threadIdx.x;
}
