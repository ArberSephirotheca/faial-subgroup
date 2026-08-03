// A class offering a const and a non-const accessor, which is what a class
// offering an accessor at all usually looks like. Both declarations share a
// name and an arity and differ only in the trailing const, so a call that
// named neither of them could not be told which it reached, and the read
// through the accessor was dropped while the direct write stayed.
struct Acc {
  float *data_;
  __device__ float get(long i) { return data_[i]; }
  __device__ float get(long i) const { return data_[i]; }
};

__global__ void k(Acc r, float *out) {
  out[threadIdx.x] = r.get(0) * 2.0f;
  r.data_[0] = 1.0f * threadIdx.x;
}
