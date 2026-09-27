// sizeof reads the operand's own type, and [1L] is a long, so this is 8
// and the index is threadIdx.x % 8. With blockDim.x = 8 every thread
// writes its own cell and the kernel is race free. Answering the operand
// with the width of a plain int gives threadIdx.x % 4, which collides
// threads 0 and 4 on y[0].
__global__ void k(int *y) {
  y[threadIdx.x % sizeof(1L)] = threadIdx.x;
}
