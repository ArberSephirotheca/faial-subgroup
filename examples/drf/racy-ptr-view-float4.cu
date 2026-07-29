// The widening view as vectorised CUDA writes it: a float4 view of a
// float array moves the pointer sixteen bytes at a time, so one store
// covers four elements and collides with a direct write to the last of
// them.
__global__ void k(float *A) {
  float4 *p = (float4 *)A;
  if (threadIdx.x == 0) p[0] = make_float4(1.0f, 2.0f, 3.0f, 4.0f);
  if (threadIdx.x == 1) A[3] = 7.0f;
}
