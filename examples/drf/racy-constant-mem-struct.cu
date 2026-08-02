struct Params { int *data; };

__constant__ Params p;

__global__ void k(int *A) {
  A[threadIdx.x] = threadIdx.x;
  p.data[0] = threadIdx.x;
}
