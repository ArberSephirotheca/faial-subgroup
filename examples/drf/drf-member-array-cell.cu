// The twin of racy-member-array-cell.cu, where the cell in the middle is
// what separates the threads. Reaching that verdict at all needs the
// subscript to be an index rather than part of the name.
struct M { float e[3][3]; };
struct S { M link[4]; };

__global__ void k(S *a) {
  if (threadIdx.x < 4) a[0].link[threadIdx.x].e[0][0] = threadIdx.x;
}
