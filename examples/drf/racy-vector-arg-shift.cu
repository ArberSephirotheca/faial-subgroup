// Shifting a vector pointer counts objects, so [a + 4] bound to a
// [double2 *] parameter starts at the fifth object and the callee's
// [t[0].x] is [a.x[4]], which is what the direct write below reaches.
// Each lane array holds one cell per object, so the shift is scaled by
// the lane's own cell and not by the object's.
__device__ void put(double2 *t) { t[0].x = 1.0; }

__global__ void k(double2 *a) {
  if (threadIdx.x == 0) put(a + 4);
  if (threadIdx.x == 1) a[4].x = 2.0;
}
