// A vector overload applying the scalar of the same name to each field.
// The scalar is declared and never defined, so nothing in the file answers
// for it, and the vector overload is the only other candidate: taking it
// would make the function call itself.
extern __host__ __device__ float sq(float x);

struct v2 { float x, y; };

__host__ __device__ v2 sq(v2 v) {
  v2 r;
  r.x = sq(v.x);
  r.y = sq(v.y);
  return r;
}

__global__ void k(v2 *out, const v2 *in) {
  out[threadIdx.x] = sq(in[0]);
}
