// A table of pointers held as a member. An object passed by value is
// copied per thread, so an array member is the thread's own storage and
// was dropped for that reason; a table of pointers is copied too, and
// what its cells hold is still the caller's memory.
struct Tab { int *d[2]; };

__global__ void k(Tab a) { a.d[0][0] = threadIdx.x; }
