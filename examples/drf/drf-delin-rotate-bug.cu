// Regression test for a bug in [drf/lib/delinearize.ml]'s
// [Expr.( - )] polynomial-subtraction primitive that mis-signed
// remainder terms when delinearising indices containing [Minus]
// subexpressions, fixed in this commit. The kernel shape mirrors
// the per-layer loop of an in-place square matrix rotation: each
// thread iterates an index [i] over a per-thread half-open range
// [[layer, n-1-layer)] and writes [matrix[layer*n + i]] alongside
// reading [matrix[i*n + (n-1-layer)]]. Both indices are of the
// form [r*n + c] with [r, c] in [0, n)] for every reachable
// (layer, i), so the natural delinearised shape is [n, n] and the
// two accesses land at flat addresses [layer*n + i] and
// [i*n + (n-1-layer)] which are statically distinct integers under
// every valid (layer, i) combination. Hence the kernel is DRF.
//
// Pre-fix, under [--all-dims --assume-dims --assume-delin] the
// delin pass rewrote the read's polynomial form
// [i*n + (n-1) - layer] into [i*n + (n+1) + layer] (both
// [Minus]es' RHS coefficients lost their sign) and the resulting
// indices [[1+i, 1+layer]] aliased the write's [[layer, i]] for
// witnesses like [n = 4], [blockDim.x = 2],
// [T1 = (tid=0, i=0)], [T2 = (tid=1, i=1)]: source-flat addresses
// for that witness are 0, 3, 5, 6 across the four accesses (no
// pair shares a cell), but delin's mis-signed form put [T1]'s
// read at the same delinearised tuple as [T2]'s write.
//
// [n] is bounded via [__assume] so the bare flat-arithmetic
// (no-delin) run finishes promptly instead of timing out; pre-fix
// the two envelopes diverged ([drf] without delin, [racy] with
// delin), and post-fix both report [drf].

__global__ void k(float *matrix, int n) {
  __assume(blockDim.y == 1 && blockDim.z == 1);
  __assume(gridDim.y == 1 && gridDim.z == 1);
  __assume(n >= 2 && n <= 8);
  int layer = blockIdx.x * blockDim.x + threadIdx.x;
  for (int i = layer; i < n - 1 - layer; ++i) {
    matrix[layer * n + i] = 1.0f;
    float v = matrix[i * n + (n - 1 - layer)];
    (void)v;
  }
}
