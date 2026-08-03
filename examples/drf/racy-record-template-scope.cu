// The same two specialisations with threads sharing an element within one
// of them. The two records stay apart, so the race is inside Tpl<int>::In
// rather than between the two.
template <typename T> struct Tpl { struct In { T v[4]; }; };

__global__ void k(Tpl<int>::In *a, Tpl<float>::In *b) {
  a->v[threadIdx.x % 4] = threadIdx.x;
  b[threadIdx.x].v[0] = 1.0f;
}
