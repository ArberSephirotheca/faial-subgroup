// Two atomics on one cell are serialised by the hardware and do not
// conflict, the same rule the CUDA path applies in
// drf-atomicexch-same-cell.cu. This is the only fixture exercising the
// WGSL atomic path.
@group(0) @binding(0) var<storage, read_write> counter: array<atomic<u32>>;

@compute @workgroup_size(32) fn computeSomething(
  @builtin(local_invocation_id) threadIdx : vec3<u32>
) {
  atomicAdd(&counter[0], 1u);
}
