// The same nested record with a per-thread element, so the member accesses
// stay disjoint once the record resolves.
struct Outer { struct Inner { float g[4]; }; };

__global__ void k(Outer::Inner *z) {
  z[threadIdx.x].g[0] = 2.0f;
}
