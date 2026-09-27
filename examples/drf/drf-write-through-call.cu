// The twin of racy-write-through-call.cu, where each thread stores through
// the accessor at a cell of its own. The index has to survive the address
// the reference return hands back, or every thread would land on the same
// cell and the pair would be reported as a race.
struct Acc {
  float *data_;
  __device__ float &operator[](long i) { return data_[i]; }
};

__global__ void k(Acc r) {
  r[threadIdx.x] = 1.0f;
}
