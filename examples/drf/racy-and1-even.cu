// Negating the low-bit test admits exactly the even threads. With
// blockDim.x = 3 the even threads are {0, 2}, so threads 0 and 2 both
// write y[0] with unsynchronised payloads 0 and 2.
__global__ void k(int *y) {
  int i = threadIdx.x;
  if (!(i & 1)) y[0] = i;
}
