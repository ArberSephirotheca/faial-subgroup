// An overloaded subscript read in expression position. The call is a value
// the surrounding expression consumes, so nothing lifted it into a statement
// and the accessor's read of the pointer member was lost: the kernel reported
// only the direct write and answered race-free.
struct Acc {
  float *data_;
  __device__ float &operator[](long i) { return data_[i]; }
};

__global__ void k(Acc r, float *out) {
  out[threadIdx.x] = r[0] * 2.0f;
  r.data_[0] = 1.0f * threadIdx.x;
}
