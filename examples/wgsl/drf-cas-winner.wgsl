// Winner uniqueness for atomicCompareExchangeWeak, the WGSL counterpart
// of drf-cas-winner.cu. At most one thread per address sees the
// exchange succeed, so the write guarded by old_value == -1 cannot
// race. The result is a struct, so the contract only reaches the guard
// because the lowering binds its old_value field as the atomic's
// target.
@group(0) @binding(0) var<storage, read_write> keys: array<atomic<i32>>;
@group(0) @binding(1) var<storage, read_write> values: array<i32>;

@compute @workgroup_size(32) fn computeSomething(
  @builtin(local_invocation_id) threadIdx : vec3<u32>
) {
  let res = atomicCompareExchangeWeak(&keys[0], -1, i32(threadIdx.x));
  if (res.old_value == -1) {
    values[0] = i32(threadIdx.x);
  }
}
