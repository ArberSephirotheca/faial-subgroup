// expected: well-synchronized
//
// Branch on threadIdx alone. The guard mentions only architectural state,
// which is shared between T1 and T2, so each thread's reach decision is
// fixed by its tid.

__global__ void k(int *data, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (threadIdx.x % 2 == 0) {
        __syncthreads();
        if (idx < n) data[idx] += 1;
    }
}
