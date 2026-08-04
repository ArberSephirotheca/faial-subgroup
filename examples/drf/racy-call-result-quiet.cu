// The store through what a call returned is the one that goes missing,
// and one unrelated store keeps the zero-accesses warning quiet, so the
// kernel comes back race-free on the accesses that remain rather than
// saying anything about the one it lost.
struct Acc { float *p; __device__ float *data() { return p; } };

__global__ void k(Acc a, float *B) {
  B[threadIdx.x] = threadIdx.x;
  a.data()[0] = threadIdx.x;
}
