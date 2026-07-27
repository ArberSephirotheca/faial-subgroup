// The unsigned companion of racy-and1-even.cu. With blockDim.x = 3 the
// guard admits threads 0 and 2, which both write y[0] with
// unsynchronised payloads 0 and 2.
__global__ void k(int *y) {
  unsigned int i = threadIdx.x;
  if (!(i & 1)) y[0] = i;
}
