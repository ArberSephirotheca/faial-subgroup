// expected: well-synchronized
//
// Named barrier with a per-thread computed ID, but reach is unconditional:
// every thread executes the bar.sync exactly once. Tests that the
// algorithm doesn't get confused by non-constant barrier IDs (it shouldn't,
// since IDs aren't part of the reach decision).

__device__ __forceinline__ void named_bar_sync(int bar_id, int count) {
    asm volatile("bar.sync %0, %1;\n" :: "r"(bar_id), "r"(count));
}

__global__ void k() {
    named_bar_sync(threadIdx.x % 4, 32);
}
