// Kernel that races regardless of launch dims: every thread writes to
// out[0]. The launch supplies a 32-thread block, but pinning
// blockDim.x = 32 doesn't rescue the kernel. Confirms --assume-launch
// does not accidentally suppress real races.
__global__ void racy_at_zero(int *out) {
    out[0] = threadIdx.x;
}

void run(int *out) {
    racy_at_zero<<<1, 32>>>(out);
}
