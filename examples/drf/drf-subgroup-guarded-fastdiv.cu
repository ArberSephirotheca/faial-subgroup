#include <cuda_fp16.h>

struct Entry {
    union {
        half2 pair;
        short fields[2];
    };
};

__global__ void kernel(Entry *out, uint3 divisor) {
    __syncwarp();
    unsigned row = (__umulhi(blockIdx.x, divisor.x) + blockIdx.x) >> divisor.y;
    unsigned index = row * 64 + threadIdx.x;
    if (index % 32 > 0) {
        return;
    }
    out[index / 32].pair = make_half2(1, 2);
}
