// An atomic whose target is the address of a name rather than of a
// cell. Read as written the argument is an address, which is not a
// spelling the target walk knows, so the builtin fell out of the atomic
// rewrite and became a call with no body: the kernel was declined for a
// missing definition that does not exist.
__device__ int counter;

__global__ void k(int *A) {
  int i = atomicAdd(&counter, 1);
  A[i] = threadIdx.x;
  A[0] = threadIdx.x;
}
