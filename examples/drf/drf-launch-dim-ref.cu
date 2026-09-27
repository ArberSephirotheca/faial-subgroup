// A launch dimension taken from a host parameter held by mutable reference.
// The dim3 decomposition asks whether each slot is an integer, and a
// reference answers no, which drops all three axes of that dimension rather
// than just the one written. With blockDim.y unpinned, two threads sharing
// threadIdx.x write p[threadIdx.x] with different values and the kernel is
// reported racy.
__global__ void k(float *p) { p[threadIdx.x] = threadIdx.y; }

void byref(float *p, int &n) { k<<<1, n>>>(p); }
