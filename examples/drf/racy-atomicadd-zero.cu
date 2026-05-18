// Negative companion to drf-atomicadd-slot.cu: zero delta makes the
// atomicAdd a fetch-without-update, so all threads see the same
// returned value and the downstream slot writes alias on the same
// cell.
__global__ void k(int *counter, int *out) {
    int slot = atomicAdd(counter, 0);
    out[slot] = threadIdx.x;
}
