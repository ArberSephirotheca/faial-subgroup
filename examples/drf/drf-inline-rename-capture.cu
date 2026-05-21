// Regression fixture for a capture-blind alpha-rename in
// [Imp.Scoped.Code.Distinct.distinct]. The kernel [k] declares
// [int i = tid + bid*bdim] and writes [out[i]]. It calls the
// helper [absq] whose parameter [i] clashes with the kernel's
// [i]. The launch sits inside a host [for (int i = ...)] loop, so
// under [--assume-launch] the synthesised launch wrapper also has
// a parameter [i] (the host-loop index).
//
// Without the fix the inline pipeline pairs the [int i] decls
// across the three scopes against each other as follows:
//
//   1. [absq] is inlined into [k]. The helper's [i] param clashes
//      with [k]'s [i], so the param is renamed to [i1] and the
//      helper body is substituted accordingly. [k]'s [out[i]]
//      reference is in the inlined-call continuation, which is
//      grafted at the leaves of the helper body via [add_inside],
//      so [out[i]] keeps the unsubstituted [i].
//
//   2. [k] is inlined into the launch wrapper. The wrapper-pass's
//      [vars_distinct] walks [k]'s body and sees [decl int i]
//      clash with the wrapper's [i]. It picks fresh name [i1] by
//      consulting only the names bound on the path from the root,
//      missing the [decl float i1] that step (1) introduced
//      deeper in the body. [subst i -> i1] then rewrites [out[i]]
//      into [out[i1]], and the walk later reaches the helper's
//      [decl float i1], which now clashes with the kernel's
//      newly-renamed [i1] and is itself renamed to [i11] with
//      [subst i1 -> i11] over its body. That body includes the
//      [out[i1]] reference from a moment earlier, so the access
//      ends up as [out[i11]] pointing at the helper's
//      uninitialised float local. The prover picks
//      [i11 = 0] for any pair of threads and reports a false
//      same-cell write.
//
// Each thread should write to a distinct cell of [out]. The fix:
// when [Distinct.distinct] picks a fresh name, the rejection set
// must include every name mentioned in the binder's body, not just
// the names bound on the path from the root.

__device__ float absq(float r, float i) {
  return r * r + i * i;
}

__global__ void k(char *out, int n) {
  int i = threadIdx.x + blockIdx.x * blockDim.x;
  if (i >= n) return;
  out[i] = (char) absq(1.0f, 2.0f);
}

void host(char *d, int n, int repeat) {
  for (int i = 0; i < repeat; i++) {
    k<<<dim3((n + 255) / 256), dim3(256)>>>(d, n);
  }
}
