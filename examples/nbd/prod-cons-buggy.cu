//xfail:BOOGIE_ERROR
//--blockDim=[64,1,1] --gridDim=[8,1,1]
// Buggy variant of prod-cons-named-bar.cu — should be flagged by faial-nbd.
//
// The producer has TWO conditional waits on the empty-stage barrier with
// off-by-one-different conditions. The intended condition is `t >= STAGES`
// (only wait once the ring has cycled); the developer originally wrote
// `t > STAGES` (off-by-one), realised the mistake, added the corrected
// guard, but forgot to delete the original.
//
// The result is two parked tasks at the same syntactic barrier id
// `bar[EMPTY_BAR_BASE + s]` with mutually-exclusive δ:
//
//     T1: δ ∧ (t >  STAGES)         -- the leftover
//     T2: δ ∧ (t == STAGES)         -- the new branch reaches it only at t = STAGES
//                                      because the leftover already swallowed t > STAGES
//
// At iteration t = STAGES the two paths disagree on whether to participate
// in the rendezvous, so faial-nbd reports a bd failure.

#include <cuda_runtime.h>

#define STAGES 3
#define PROD_THREADS 32
#define CONS_THREADS 32
#define BLOCK_THREADS (PROD_THREADS + CONS_THREADS)

#define FULL_BAR_BASE  1
#define EMPTY_BAR_BASE 4

__device__ __forceinline__ void named_bar_sync(int bar_id, int count) {
    asm volatile("bar.sync %0, %1;\n" :: "r"(bar_id), "r"(count));
}

__device__ __forceinline__ void named_bar_arrive(int bar_id, int count) {
    asm volatile("bar.arrive %0, %1;\n" :: "r"(bar_id), "r"(count));
}

__global__ void ws_skeleton_kernel_buggy(const float *in, float *out, int num_tiles) {
    __requires(blockDim.x == 64);
    __requires(blockDim.y == 1);
    __requires(blockDim.z == 1);

    __shared__ float buf[STAGES][32];

    const int tid  = threadIdx.x;
    const int lane = tid & 31;
    const bool prod = tid < PROD_THREADS;

    if (prod) {
        for (int t = 0; t < num_tiles; ++t) {
            const int s = t % STAGES;

            // -- BUG: refactoring leftover. The old (off-by-one) wait was
            //    never deleted when the corrected one was added below.
            if (t > STAGES) {
                named_bar_sync(EMPTY_BAR_BASE + s, BLOCK_THREADS);
            }
            // -- The intended wait.
            if (t >= STAGES) {
                named_bar_sync(EMPTY_BAR_BASE + s, BLOCK_THREADS);
            }

            buf[s][lane] = in[t * 32 + lane];

            named_bar_arrive(FULL_BAR_BASE + s, BLOCK_THREADS);
        }
    } else {
        for (int t = 0; t < num_tiles; ++t) {
            const int s = t % STAGES;

            named_bar_sync(FULL_BAR_BASE + s, BLOCK_THREADS);

            out[t * 32 + lane] = buf[s][lane];

            named_bar_arrive(EMPTY_BAR_BASE + s, BLOCK_THREADS);
        }
    }
}
