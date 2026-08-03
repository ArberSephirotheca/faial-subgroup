// A method called on an object, which clang emits with the method named by a
// member selection rather than by a declaration reference. The signature was
// looked up by declaration, so the call resolved to nothing and the read of
// the pointer member inside the method never happened.
struct Acc {
  float *data_;
  __device__ float get(long i) { return data_[i]; }
};

__global__ void k(Acc r) {
  float v = r.get(0);
  r.data_[threadIdx.x] = v;
}
