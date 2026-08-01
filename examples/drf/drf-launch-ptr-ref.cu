// A launch whose pointer argument is a host parameter taken by mutable
// reference. The reference belongs to the wrapper's signature: the launch
// passes the pointer's value, and the synthesised pseudo-kernel only reads
// the variable, so its parameter is the referent's type. Carrying the
// reference through leaves the parameter unsupported, no array registers,
// and every access in the kernel is dropped.
__global__ void k(float *p) { p[threadIdx.x] = 1; }

void byref(float *&p) { k<<<1, 32>>>(p); }
