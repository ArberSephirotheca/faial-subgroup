// Regression fixture for the loop-normalization pass in
// [Drf.Flatacc.from_loc_split] / [Drf.Unsynced.normalize_loops] /
// [Protocols.Range.normalize]. The kernel shape mirrors a
// per-warp half-vector pattern: each thread starts at
// [y = threadIdx.x * 2], iterates with stride [blockDim.x * 2],
// and writes to [src[y / 2]] (the [/ 2] models a [(half2*)src]
// cast's [>> 1] address arithmetic, which appears in the
// post-[__assume] IR as integer division by two). With
// [blockDim.x] pinned at 64, the per-iteration index for thread
// [t] at iteration [k] is [(2*t + 128*k) / 2 = t + 64*k]; for
// any pair [t1 != t2] in [[0, 64)] the indices differ by [t1 -
// t2], so the kernel is DRF.
//
// Pre-normalization the stride [blockDim.x * 2] reduces to
// [Num 128] under [--assume-dims] and [Range.to_cond]'s [Plus n]
// arm emits [(y - 2*threadIdx.x) % 128 == 0]; combined with the
// access's [/ 2] the race goal mixes integer modulo with integer
// division over symbolic operands, which lands on Z3's
// non-linear-int tactic and does not return inside the standard
// per-query timeout. Post-normalization the loop binder becomes
// a fresh quotient [q] over [[0, ...]] step 1, the original
// iteration variable is substituted by [2*threadIdx.x + 128*q],
// and the modulo constraint disappears from the SMT query; the
// race goal is linear in [(t, q)] and decides quickly.

__global__ void k(float *src) {
  __assume(blockDim.x == 64 && blockDim.y == 1 && blockDim.z == 1);
  __assume(gridDim.x == 1 && gridDim.y == 1 && gridDim.z == 1);
  int y = threadIdx.x * 2;
  for (; y < 1024; y = y + blockDim.x * 2) {
    src[y / 2] = 1.0f;
  }
}
