// Two members are two arrays, so a write to [C[i].key] and a write to
// [C[i].val] never meet, and the per-thread element index keeps each
// array's own writes apart.
struct Cell { int key; int val; };

__global__ void k(Cell *C) {
  C[threadIdx.x].key = 1;
  C[threadIdx.x].val = 2;
}
