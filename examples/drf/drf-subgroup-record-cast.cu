struct Entry { int word; };

__global__ void kernel(int *out) {
    __shared__ char bytes[128];
    Entry *entries = (Entry *)bytes;
    entries[threadIdx.x].word = threadIdx.x;
    __syncthreads();
    out[threadIdx.x] = entries[0].word;
    __syncwarp();
}
