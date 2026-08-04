// A by-value parameter whose type is written through a typedef of an
// elaborated name. The record is declared as Buffer, and the desugared
// spelling is the only place that name survives, so renaming the type to
// the alias leaves the field naming no memory at all.
struct Buffer { float *data; unsigned long pitch; };
typedef struct Buffer BufferT;

__global__ void k(BufferT b) {
  b.data[0] = threadIdx.x;
}
