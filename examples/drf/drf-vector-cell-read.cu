// The twin of racy-vector-cell-read.cu with each thread on its own cell,
// so the lanes the read expands into are the ones the same thread writes.
__global__ void k(double2 *a) {
  double2 v = a[threadIdx.x];
  a[threadIdx.x].x = v.y;
}
