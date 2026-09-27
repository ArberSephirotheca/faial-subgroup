__global__ void mul_chain(float *a, int reps, float v) {
  int t = threadIdx.x + blockIdx.x * blockDim.x;
  float s = a[t];
  for (int j = 0; j < reps; j++) {
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
    s = s * s * v;
  }
  a[t] = s;
}
