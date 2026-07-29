// A pointer view narrower than the array's element indexes in its own
// units, so p[1] is byte 1 of A, which lives in element 0. Dropping the
// view would read the 1 as an element index and put the two writes in
// different cells, reporting a kernel that races as data-race free.
__global__ void k(int *A) {
  char *p = (char *)A;
  if (threadIdx.x == 0) p[1] = 1;
  if (threadIdx.x == 1) A[0] = 7;
}
