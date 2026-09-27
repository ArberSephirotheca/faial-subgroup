// A [const] by-value struct parameter of the kernel itself, supplied by a
// launch site. The synthesised wrapper expands the argument from the host
// variable's unqualified type, so the kernel's parameter has to expand too
// or every parameter after it, [p] among them, binds to the wrong argument
// and the write is retargeted onto a scalar.
typedef struct P { int a; int b; } P;

__global__ void k(const P c, float *p) { p[threadIdx.x] = 1; }

void run(P c, float *p) { k<<<1, 32>>>(c, p); }
