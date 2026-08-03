// The same namespaced record with a per-thread element, so the member
// accesses stay disjoint once the record resolves.
namespace a { struct S { float f[4]; }; }

__global__ void k(a::S *x) {
  x[threadIdx.x].f[0] = 1.0f;
}
