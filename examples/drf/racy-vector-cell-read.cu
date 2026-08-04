// Reading a whole vector cell reads every lane of it. A vector pointer is
// decomposed into one array per lane, so the object holding the vector
// names no memory and only its lanes do; leaving the read whole names a
// region that does not exist and the access goes missing. Here the lane
// every thread reads is the lane thread one writes.
__global__ void k(double2 *a) {
  double2 v = a[1];
  a[threadIdx.x].x = v.y;
}
