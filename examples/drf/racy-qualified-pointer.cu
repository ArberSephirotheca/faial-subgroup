// Pointer parameter with the [volatile T * const __restrict]
// qualifier stack — the combination CUDA frequently uses on
// hot-path device buffers.
//
// Without normalisation of the qualifier suffix, c_type's pointer
// detection misses this shape; the IR builder classifies the
// parameter as Unsupported, and every read/write through it lowers
// to skip. The kernel below would then false-negatively report
// DRF, even though every thread writes the same address.
//
// With normalisation, the parameter is recognised as a global
// array and the unconditional write to out[0] is detected as a
// CIDI race.
__global__ void racy_qualified(volatile int* const __restrict out) {
    out[0] = threadIdx.x;
}
