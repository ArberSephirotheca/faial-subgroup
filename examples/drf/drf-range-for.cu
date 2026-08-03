// C++11 range-based [for (T v : arr)] over a fixed-size built-in
// array. The bound [N] is recoverable from the synthetic [__rangeN]
// declaration's qualType ([int (&)[N]]); the parser uses it to emit
// a bounded loop with [v = arr[__idx]] in the body so the analyzer
// sees a concrete [foreach 0 <= __idx < N] instead of an unbounded
// [Star]. Single-thread guard keeps the kernel DRF regardless.
__global__ void range_for_fixed_array(int *out) {
    int arr[4] = {10, 20, 30, 40};
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx == 0) {
        for (int v : arr) {
            out[v] = 1;
        }
    }
}
