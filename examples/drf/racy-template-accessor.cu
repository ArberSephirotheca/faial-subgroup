// An accessor written as a class template, which is how a tensor library
// spells one. A use of the type carries its template arguments while the
// declaration registers under the bare name, so the parameter resolved to
// no record and its pointer member named no region.
template <typename T>
struct Acc {
  T *data_;
  __device__ T &operator[](long i) { return data_[i]; }
};

__global__ void k(Acc<float> r, float *out) {
  out[threadIdx.x] = r[0] * 2.0f;
  r.data_[0] = 1.0f * threadIdx.x;
}
