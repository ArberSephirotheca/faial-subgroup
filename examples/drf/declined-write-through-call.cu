// Assigning through what a call returns. The reference names a location,
// but a function is analyzed for the value it returns, and the returned
// subscript has already been lifted into a read by the time anyone could
// use it as a target. The assignment used to vanish, leaving a kernel whose
// only write was gone and whose remaining reads answered race-free.
struct Acc {
  float *data_;
  __device__ float &operator[](long i) { return data_[i]; }
};

__global__ void k(Acc r) {
  r[0] = 1.0f * threadIdx.x;
}
