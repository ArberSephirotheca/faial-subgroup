// expected: missing participants.
//
// A named barrier that requires 32 participants, executed in a block
// configured with only 16 threads. The cohort caps at 16 (the block
// size); the barrier's count parameter is 32 → undersized.
//
// Run with --block-dim=16 to expose the configuration.

__device__ __forceinline__ void named_bar_sync_32() {
    asm volatile("bar.sync 0, 32;\n");
}

__global__ void k() {
    named_bar_sync_32();
}
