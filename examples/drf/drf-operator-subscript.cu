// The same accessor with every thread on its own element. A lifted call that
// dropped the subscript would read cell zero in every thread and meet the
// write, so a race-free verdict is what says the index survived the lift.
struct Acc {
  float *data_;
  __device__ float &operator[](long i) { return data_[i]; }
};

__global__ void k(Acc r) {
  float v = r[threadIdx.x];
  r.data_[threadIdx.x] = v + 1.0f;
}
