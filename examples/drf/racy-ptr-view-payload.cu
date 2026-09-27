// The same collision with the same literal stored on both sides. Two
// writes of one literal are taken not to conflict, which holds while a
// byte write and an element write sit in different cells and stops
// holding once the view puts them in the same one: storing 1 as a byte
// does not store the bits that storing 1 as an int does. So a scaled
// access has to give up its payload, and this is the only example where
// keeping it is visible.
__global__ void k(int *A) {
  char *p = (char *)A;
  if (threadIdx.x == 0) p[1] = 1;
  if (threadIdx.x == 1) A[0] = 1;
}
