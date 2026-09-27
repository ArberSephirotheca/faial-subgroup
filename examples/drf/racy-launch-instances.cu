// One launch line reached from two instantiations of the enclosing template.
// The two records differ only in the block dimension the template argument
// supplies, and they share a callee and a line, so the second overwrites the
// first. The 32-thread instantiation races on a[0]; the single-thread one
// cannot, and analyzing only the last reports the file race-free.
__global__ void k(int *a) { a[0] = threadIdx.x; }

template <int BS>
void launch(int *d) { k<<<1, BS>>>(d); }

void run(int *d) { launch<32>(d); launch<1>(d); }
