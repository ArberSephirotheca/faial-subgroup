/*

Example: divergent barrier

Here only even threads are synchronized
*/

#include <stdio.h>

__global__ void divergent_barrier_kernel(int * data, int n) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    // Some computation
    if (idx < n) {
        data[idx] = idx * 2;
    }

    if (threadIdx.x % 2 == 0) {
        __syncthreads();

        if (idx < n) {
            data[idx] += 1;
        }
    }
}
