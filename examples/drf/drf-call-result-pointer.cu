// A callee handing back a pointer hands back a location. The parameter
// is named apart from the caller's array on purpose: the returned
// expression is in the callee's terms, so a binding the argument
// substitution cannot reach resolves onto a name that is not memory.
__device__ float *pick(float *q) { return q; }

__global__ void k(float *p) { pick(p)[threadIdx.x] = threadIdx.x; }
