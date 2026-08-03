// The same pair of same-named records with a per-thread element, so the
// member accesses stay disjoint once each record keeps its own fields.
struct S { float f[4]; };
namespace a { struct S { int *p; }; }

__global__ void k(S *y) {
  y[threadIdx.x].f[0] = 1.0f;
}
