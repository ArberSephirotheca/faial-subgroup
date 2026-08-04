// The twin of drf-unseen-overload.cu with every thread on one output cell.
extern __host__ __device__ float sq(float x);

struct v2 { float x, y; };

__host__ __device__ v2 sq(v2 v) {
  v2 r;
  r.x = sq(v.x);
  r.y = sq(v.y);
  return r;
}

__global__ void k(v2 *out, const v2 *in) {
  out[0] = sq(in[threadIdx.x]);
}
