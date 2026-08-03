// The same out-of-line member class with a per-thread element. Without the
// record resolving there are no accesses at all, so this is the half that
// fails when the scope is not recovered.
struct Outer { struct Inner; int *r; };
struct Outer::Inner { float g[4]; };

__global__ void k(Outer::Inner *z) {
  z[threadIdx.x].g[0] = 1.0f;
}
