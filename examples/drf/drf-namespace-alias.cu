namespace foo { __device__ int bar(){ return 0; } }

__global__ void k(int * d) {
    namespace baz = foo;
    d[threadIdx.x] = baz::bar();
}
