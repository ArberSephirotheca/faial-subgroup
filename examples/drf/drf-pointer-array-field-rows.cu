// Two rows of the table are separate memory, so the two stores do not
// meet. Dropping the row index would land both on one cell and report a
// race the kernel cannot have.
struct Tab { int *d[2]; };

__global__ void k(Tab a) {
  if (threadIdx.x == 0) a.d[0][0] = 1;
  if (threadIdx.x == 1) a.d[1][0] = 2;
}
