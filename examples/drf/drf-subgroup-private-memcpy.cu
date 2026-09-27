#include <string.h>

__global__ void kernel(float *out) {
    int bits = threadIdx.x;
    float value;
    memcpy(&value, &bits, sizeof(float));
    __syncwarp();
    out[threadIdx.x] = value;
}
