// [divUp] is the third entry with a body. Dividing by one is the
// identity, so each thread writes its own cell. The kernel declares
// divUp rather than defining it, because a definition would be
// inlined and the registry entry would never be consulted.
__device__ int divUp(int a, int b);

__global__ void k(int *out) {
  int t = threadIdx.x;
  out[divUp(t, 1)] = t;
}
