struct View { void *ptr; unsigned long pitch; };

__global__ void k(View v, int width, int *B) {
  int x = threadIdx.x;
  B[x] = 1;
  char *base = (char *)v.ptr;
  float *row = (float *)(base + v.pitch);
  row[x % width] = 1.0f;
}
