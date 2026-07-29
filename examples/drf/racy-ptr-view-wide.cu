// A pointer view wider than the array's element covers several elements
// per access, so p[0] is bytes 0 to 3 of A and meets a direct write to
// byte 3. Modelling the wide access as a single element would cover only
// the first of the four and lose the collision.
__global__ void k(char *A) {
  int *p = (int *)A;
  if (threadIdx.x == 0) p[0] = 1;
  if (threadIdx.x == 1) A[3] = 7;
}
