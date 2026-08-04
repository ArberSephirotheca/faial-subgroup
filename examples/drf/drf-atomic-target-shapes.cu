// Every way an atomic's target can be written, in one kernel. An address
// the rewrite cannot read leaves the builtin standing as a call with no
// body, so a single spelling regressing declines the whole kernel and
// this file moves from race-free to unsupported. All targets are
// distinct memory, and atomics do not conflict with one another, so the
// verdict says only that each one was recognised.
struct P { int *ptr; int cells[4]; int scalar; };

__device__ int g_scalar;
__device__ int g_cells[4];
__device__ int *g_ptr;

__global__ void k(int *c, P *p, P s, int **q, int j, int n) {
  __shared__ int slot;
  atomicAdd(c, 1);              // a pointer
  atomicAdd(&c[0], 1);          // the address of its first cell
  atomicAdd(&c[j], 1);          // the address of a cell
  atomicAdd(c + j, 1);          // a pointer plus an offset
  atomicAdd(&g_scalar, 1);      // the address of a name
  atomicAdd(g_cells, 1);        // an array, decayed
  atomicAdd(&g_cells[j], 1);    // the address of one of its cells
  atomicAdd(g_ptr, 1);          // a pointer held in a global
  atomicAdd(p->ptr, 1);         // a pointer member, through an arrow
  atomicAdd(&p->ptr[0], 1);     // the address of the cell it points at
  atomicAdd(p->cells, 1);       // an array member, decayed
  atomicAdd(&p->cells[j], 1);   // the address of one of its cells
  atomicAdd(&p->scalar, 1);     // the address of a scalar member
  atomicAdd(s.ptr, 1);          // a pointer member of a by-value object
  atomicAdd(&p[n].cells[j], 1); // a member of an indexed object
  atomicAdd(q[0], 1);           // a pointer read out of a table
  atomicAdd(&slot, 1);          // the address of a shared scalar
}
