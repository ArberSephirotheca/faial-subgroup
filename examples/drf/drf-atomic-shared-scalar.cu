// A shared scalar reaches the same place by another road: the front end
// already spells its accesses as a one-cell array, so [&slot] arrives as
// the address of a cell and resolved even when the address of a name did
// not. Here to hold the two spellings together.
__global__ void k(int *A) {
  __shared__ int slot;
  if (threadIdx.x == 0) slot = 0;
  __syncthreads();
  int i = atomicAdd(&slot, 1);
  A[i] = threadIdx.x;
}
