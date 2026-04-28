// expected: oversize cohort.
//
// A named barrier with a count of 32 executed unconditionally in a
// 64-thread block. The cohort is the full block (every thread reaches
// the asm), so 64 threads arrive at a barrier expecting 32 — oversize.
// Exercises the sub-warp [exceeds_cardinality] path: small-distinctness
// SAT pins 33 distinct tids that all satisfy the cohort, witnessing the
// bug without enumerating the full 64-way count.
//
// Run with --block-dim=64 to expose the configuration.

__device__ __forceinline__ void named_bar_sync_32() {
    asm volatile("bar.sync 0, 32;\n");
}

__global__ void k() {
    named_bar_sync_32();
}
