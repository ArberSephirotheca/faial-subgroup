// A scalar kernel argument fed by a host-side [const int = N]
// variable that c-to-json constant-folds to its literal value at
// the launch site. Without literal pass-through in the launch-arg
// resolver, the folded [IntegerLiteral N] reaches the kernel call
// as a fresh symbolic uniform ([__faial_launch_arg_0]) with no
// bound, the formal [stride] inlines to that free symbol, and Z3
// witnesses [stride == 0] — every block then collides on
// [threadIdx.x] and the kernel false-positive reports racy.
//
// With pass-through, the call inlines as [stride_write(d_y,
// 256)]; the formal [stride] becomes the literal [256] throughout
// the kernel body, the index expression resolves to [blockIdx.x *
// 256 + threadIdx.x], and the kernel analyzes DRF (each
// (blockIdx.x, threadIdx.x) pair maps to a distinct address in
// [0, 4 * 256)).
__global__ void stride_write(int stride, float *y) {
    y[blockIdx.x * stride + threadIdx.x] = 1.0f;
}

void run(float *y) {
    const int N = 256;
    stride_write<<<dim3(4), dim3(256)>>>(N, y);
}
