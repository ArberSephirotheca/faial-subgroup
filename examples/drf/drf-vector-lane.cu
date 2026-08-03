// Two lanes are two arrays, so a write to [A[i].x] and a write to
// [A[i].y] never meet, and the per-thread element index keeps each
// array's own writes apart.
__global__ void k(float4 *A) {
  A[threadIdx.x].x = 1.0f;
  A[threadIdx.x].y = 2.0f;
}
