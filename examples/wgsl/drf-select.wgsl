@group(0) @binding(0) var<storage, read_write> data: array<f32>;

@compute @workgroup_size(256, 1, 1) fn computeSomething(
  @builtin(workgroup_id) blockIdx : vec3<u32>,
  @builtin(num_workgroups) gridDim : vec3<u32>,
  @builtin(local_invocation_id) threadIdx : vec3<u32>
) {
  if (gridDim.y != 1 || gridDim.z != 1) { return ; }
  let base = blockIdx.x*256u + threadIdx.x;
  let i = base * 2u + select(0u, 1u, threadIdx.x < 128u);
  data[i] = data[i] * 2.0;
}
