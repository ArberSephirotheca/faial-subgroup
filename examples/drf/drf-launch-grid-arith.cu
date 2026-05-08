// A kernel where the per-block stride is a runtime kernel arg
// ([imageW]) and the launch picks [gridDim.x = imageW / 128].
// Two threads in different blocks ([b1], [b2]) collide on
// [b1 * imageW + t1 == b2 * imageW + t2] iff
// [(b1 - b2) * imageW == t2 - t1]; with [|t2 - t1| < 128 = blockDim.x],
// that's only solvable when [imageW < 128].
//
// The launch site supplies [gridDim.x = imageW / 128]. Combined
// with the existing [gridDim.x >= 1] preamble in the verifier,
// Z3 derives [imageW >= 128] transitively. The grid-axis
// expression has to reach the SMT layer with its arithmetic
// structure intact — if [Launch_arg] over-abstracted [imageW /
// 128] into a fresh symbolic uniform, the relation would be
// hidden from Z3 and the kernel would false-positive racy.
__global__ void grid_arith_stride(int imageW, int *y) {
    y[blockIdx.x * imageW + threadIdx.x] = threadIdx.x;
}

void run(int imageW, int *y) {
    grid_arith_stride<<<dim3(imageW / 128), dim3(128)>>>(imageW, y);
}
