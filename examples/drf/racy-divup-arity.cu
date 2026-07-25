// A name the registry knows, applied at an arity the registry does
// not declare. The entry must not govern this call: its body expects
// two arguments and its declaration describes a two-argument
// function, so the call keeps its symbol and the write index stays
// free. Applying the entry regardless raises out of the body.
__device__ int divUp(int a, int b, int c);

__global__ void k(int *out) {
  int t = threadIdx.x;
  out[divUp(t, t, t)] = t;
}
