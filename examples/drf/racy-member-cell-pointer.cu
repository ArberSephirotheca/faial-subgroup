// The twin of drf-member-cell-pointer.cu, where both spellings reach the
// same cell: the arrow contributes the zero that the explicit subscript
// writes out. Naming the cell by position must still identify these two,
// or the pair above would clear for the wrong reason.
struct In  { int *p; };
struct Mid { In a[2]; };

__global__ void k(Mid *s) {
  if (threadIdx.x == 0) s[0].a[1].p[0] = 1;
  if (threadIdx.x == 1) s->a[1].p[0] = 2;
}
