struct Entry {
    union {
        int word;
        char bytes[4];
    };
};

__global__ void kernel(int *out) {
    __shared__ Entry entries[32];
    entries[threadIdx.x].word = threadIdx.x;
    int value = entries[0].bytes[0];
    __syncwarp();
    out[threadIdx.x] = value;
}
