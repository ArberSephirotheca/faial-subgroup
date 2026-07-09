// A memory read hoisted out of a ?: keeps the ternary's condition as a
// guard on the access. The read of s[tid] fires only when tid < n and
// the write to s[tid-1] only when tid > n, so the two index spaces are
// disjoint (a reader needs tid < n, a writer's cell tid-1 >= n) and the
// kernel is race free. Dropping the ternary guard models the read as
// always happening, which spuriously collides with the write.
__global__ void k(float *out, float *s, int n) {
  int tid = blockIdx.x * blockDim.x + threadIdx.x;
  if (tid > n) s[tid - 1] = 1.0f;
  float v = (tid < n) ? s[tid] : 0.0f;
  out[tid] = v;
}
