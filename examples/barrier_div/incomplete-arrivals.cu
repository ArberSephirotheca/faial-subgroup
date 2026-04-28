// expected: incomplete arrivals diagnostic.
//
// All 16 threads execute [bar.arrive 0, 32] unconditionally and
// continue. Arrivals = 16, expected = 32, no waiters → the membrane
// is stuck in a half-collected state. No deadlock yet (no thread is
// blocked), but the barrier contract is violated and any future
// [bar.wait(0, _)] anywhere in the kernel would deadlock. This is
// the split-phase analogue of [Missing_participants] for the case
// where [b_w = ⊥].
//
// Run with --block-dim=16.

__device__ __forceinline__ void named_bar_arrive_32() {
    asm volatile("bar.arrive 0, 32;\n");
}

__global__ void k() {
    named_bar_arrive_32();
}
