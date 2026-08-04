// Storing a whole vector cell writes every lane of it, so the collision
// between the threads that share a cell has to appear on both lanes and
// not on an object that names no memory.
__global__ void k(double2 *a) {
  double2 v = a[0];
  a[threadIdx.x % 4] = v;
}
