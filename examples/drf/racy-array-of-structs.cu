// An inline array member of an array element is one array whose leading
// index is the element. Before the subscript chain named it, the write was
// replaced by a read of the enclosing element, and reads do not race with
// reads, so the kernel cleared.
struct Atom { double f[3]; };

__global__ void k(Atom *s) { s[0].f[threadIdx.x % 3] = 1.0; }
