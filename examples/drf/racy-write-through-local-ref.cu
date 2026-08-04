// A reference into a member array of a by-value object, which is how a
// small vector type spells its lanes. The object is the caller's own copy,
// so the address names no memory and the store is dropped the way a
// directly spelled one is; what the kernel is judged on is the write to the
// array below.
struct V {
  float x_[3];
  __device__ float &operator[](int i) { return x_[i]; }
};

__global__ void k(float *g) {
  V f;
  f[0] = threadIdx.x;
  g[threadIdx.x % 4] = f[0];
}
