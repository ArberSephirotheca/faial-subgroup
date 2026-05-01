// expected: divergent
//
// Branch on a decl whose value the analyzer treats as unconstrained
// (loaded from input memory). The decl is projectable, so x$T1 and x$T2
// can disagree on the sign and the barrier is reached on T1 but not T2.

__global__ void k(int *data) {
    int x = data[threadIdx.x];
    if (x > 0) {
        __syncthreads();
    }
}
