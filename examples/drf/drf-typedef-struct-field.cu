// The twin of racy-typedef-struct-field.cu with each thread on a cell of
// its own.
struct Buffer { float *data; unsigned long pitch; };
typedef struct Buffer BufferT;

__global__ void k(BufferT b) {
  b.data[threadIdx.x] = threadIdx.x;
}
