// The twin of racy-atomic-member-target.cu, with the store one cell
// along. The atomic reaches cell zero and nothing else, so an index the
// bare pointer picked up from somewhere would show here.
struct P { int *ptr; };

__global__ void k(P *p) {
  if (threadIdx.x == 0) atomicAdd(p->ptr, 1);
  if (threadIdx.x == 1) p->ptr[1] = 5;
}
