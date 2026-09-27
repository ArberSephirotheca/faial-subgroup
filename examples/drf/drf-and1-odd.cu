// [i & 1] is the low bit of [i], so the guard admits exactly the odd
// threads. With blockDim.x = 3 the odd threads are {1}, so the write to
// y[0] has a single author and the kernel is race free.
__global__ void k(int *y) {
  int i = threadIdx.x;
  if (i & 1) y[0] = i;
}
