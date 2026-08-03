// Two specialisations that differ only in which declaration they name. A
// declaration argument spells its identity in the declaration it points at
// rather than in the argument itself, so rendering the arguments to text
// made both classes one record, and the write through Ptr<g> was reported
// against h. Both spellings race either way, so the verdict does not catch
// it; the array each access names does.
__device__ int g[4];
__device__ int h[4];

template <int *P> struct Ptr {
  __device__ void put(int i, int v) { P[i] = v; }
};

__global__ void k() {
  Ptr<g> a;
  Ptr<h> b;
  a.put(0, threadIdx.x);
  b.put(0, threadIdx.x);
}
