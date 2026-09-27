// The same record behind a pointer, with each thread reading the address
// out of its own holder. Two holders hold two addresses, so the writes
// are apart even though they take the same cell of the record.
struct In { double c[2]; };
struct Holder { In *b; };

__global__ void k(Holder *t) {
  t[threadIdx.x].b->c[0] = threadIdx.x;
}
