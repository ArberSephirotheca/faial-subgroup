// The twin of racy-vector-arg-shift.cu that pins where the shift lands:
// the direct write is at the object the double2 is wide, so a shift
// scaled by the object rather than by the lane would put the callee here
// and report a collision that the program does not have.
__device__ void put(double2 *t) { t[0].x = 1.0; }

__global__ void k(double2 *a) {
  if (threadIdx.x == 0) put(a + 4);
  if (threadIdx.x == 1) a[8].x = 2.0;
}
