// A reference return whose lvalue is a choice between two arrays. The
// address of a choice is not a pointer this spells, so the callee hands
// back no location and the store cannot be placed. Declining says so:
// dropping it would leave a kernel whose only write is gone.
__device__ float &pick(float *a, float *b, bool c) { return c ? a[0] : b[0]; }

__global__ void k(float *x, float *y) {
  pick(x, y, threadIdx.x == 0) = 1.0f;
}
