// Assigning through what a call returns. A function returning a reference
// hands back the address of the cell it names, so the store is a write
// through that address. While the returned subscript was lifted into a
// read instead, the assignment vanished and the kernel's only write went
// with it.
struct Acc {
  float *data_;
  __device__ float &operator[](long i) { return data_[i]; }
};

__global__ void k(Acc r) {
  r[0] = 1.0f * threadIdx.x;
}
