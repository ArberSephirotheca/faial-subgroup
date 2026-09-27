// memcpy copies bytes, and its stub's loop runs one iteration per byte,
// so the byte index has to be truncated to an element before it names a
// cell of an int array. Each thread copies exactly its own element here,
// and reading the byte count as an element count would have thread t
// cover elements t to t + 3 and collide with its neighbours.
#include <string.h>

__global__ void k(int *A, int *B) {
  int t = threadIdx.x;
  memcpy(&A[t], &B[t], sizeof(int));
}
