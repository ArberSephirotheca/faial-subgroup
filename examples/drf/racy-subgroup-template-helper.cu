template<int N>
__device__ void write_helper(int *out) {
    out[0] = threadIdx.x + N;
}

template<int N>
__global__ void kernel(int *out) {
    write_helper<8>(out);
    __syncwarp();
}
