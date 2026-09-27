// A scalar member of an array element is a cell of its own array, indexed
// by the element. Before the store recognised a member selection as a
// target, the assignment fell through to the arms that only rewrite
// subexpressions, leaving a read of the element in its place, and reads do
// not race with reads.
struct Cell { int key; int val; };

__global__ void k(Cell *C) { C[0].key = threadIdx.x; }
