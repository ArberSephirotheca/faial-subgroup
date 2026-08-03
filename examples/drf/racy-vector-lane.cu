// A lane of a vector element is a cell of its own array, indexed by the
// element, the same way a struct member is. Before the lanes were arrays
// the write was replaced by a read of the whole element, and reads do not
// race with reads.
__global__ void k(float4 *A) { A[0].x = threadIdx.x; }
