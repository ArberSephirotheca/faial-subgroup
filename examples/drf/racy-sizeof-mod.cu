// sizeof(int) is 4, so the index is threadIdx.x % 4. With blockDim.x = 8
// threads 0 and 4 both land on y[0] and write unsynchronised payloads 0
// and 4.
__global__ void k(int *y) {
  y[threadIdx.x % sizeof(int)] = threadIdx.x;
}
