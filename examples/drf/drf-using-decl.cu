namespace foo { __device__ int bar(){ return 0; } }

__global__ void k(int * d) {
    using foo::bar;
    d[threadIdx.x] = bar();
}
