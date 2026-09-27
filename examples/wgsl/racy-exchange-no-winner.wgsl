// Negative companion to drf-cas-winner.wgsl: the same kernel shape with
// atomicExchange, which carries no cross-thread contract, so nothing
// bounds how many threads see -1 and the guarded write is racy.
@group(0) @binding(0) var<storage, read_write> keys: array<atomic<i32>>;
@group(0) @binding(1) var<storage, read_write> values: array<i32>;

@compute @workgroup_size(32) fn computeSomething(
  @builtin(local_invocation_id) threadIdx : vec3<u32>
) {
  let old = atomicExchange(&keys[0], i32(threadIdx.x));
  if (old == -1) {
    values[0] = i32(threadIdx.x);
  }
}
