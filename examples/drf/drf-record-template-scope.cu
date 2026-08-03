// A record declared inside a class template. Each specialisation is a
// scope of its own, so Tpl<int>::In and Tpl<float>::In are two records
// with two member types rather than one keyed under a shared bare name.
// Each thread takes its own element, so nothing collides.
template <typename T> struct Tpl { struct In { T v[4]; }; };

__global__ void k(Tpl<int>::In *a, Tpl<float>::In *b) {
  a[threadIdx.x].v[0] = 1;
  b[threadIdx.x].v[0] = 1.0f;
}
