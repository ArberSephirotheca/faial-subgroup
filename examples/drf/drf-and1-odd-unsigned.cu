// The unsigned companion of drf-and1-odd.cu. The low bit of [i] does not
// depend on the signedness of [i], so with blockDim.x = 3 the guard again
// admits only thread 1 and the write to y[0] has a single author.
__global__ void k(int *y) {
  unsigned int i = threadIdx.x;
  if (i & 1) y[0] = i;
}
