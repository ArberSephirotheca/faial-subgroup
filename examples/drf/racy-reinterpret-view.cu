// The twin of drf-reinterpret-view.cu, with the write landing in the
// buffer the view reads. [t[1].a[i]] is [in[4 + i]], so a thread reading
// the second tree meets the thread writing that cell. Keeping the member
// in a region of its own would put the read and the write in different
// arrays and report nothing.
struct T { int a[4]; };

__device__ void put(const T *t, int *out, int i) {
  out[i] = t[1].a[i];
}

__global__ void k(int *in) {
  put((const T *)in, in, threadIdx.x % 8);
}
