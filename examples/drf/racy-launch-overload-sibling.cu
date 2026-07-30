// Two overloads share a bare name and only the int* one is launched.
// The launched overload is demoted and checked through its synthesised
// pseudo-kernel, where the launch's 32 threads write distinct slots and
// it is DRF. The float* overload is never launched, so it stays an
// entry point and is checked with free dims, where every thread writing
// a[0] races. Resolving the launch target by bare name demotes both
// overloads, leaving the float* one with neither an entry point nor a
// pseudo-kernel, and the race goes unreported.
__global__ void k(int *a) {
    a[threadIdx.x] = threadIdx.x;
}

__global__ void k(float *a) {
    a[0] = threadIdx.x;
}

void run(int *d) {
    k<<<1, 32>>>(d);
}
