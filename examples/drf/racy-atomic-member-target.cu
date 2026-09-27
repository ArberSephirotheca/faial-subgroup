// A pointer member names the region it points at, not the field holding
// the address. The atomic and the store meet on that region's first
// cell; naming the field instead would separate them and lose the race.
struct P { int *ptr; };

__global__ void k(P *p) {
  if (threadIdx.x == 0) atomicAdd(p->ptr, 1);
  if (threadIdx.x == 1) p->ptr[0] = 5;
}
