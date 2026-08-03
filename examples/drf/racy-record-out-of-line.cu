// A member class defined out of line. The definition is written at
// namespace scope while the type belongs to Outer, so the enclosing
// declarations credit it with no scope and key it Inner, against a type
// spelled Outer::Inner. The scope the record declares itself to be in is
// what matches.
struct Outer { struct Inner; int *r; };
struct Outer::Inner { float g[4]; };

__global__ void k(Outer::Inner *z) {
  z->g[threadIdx.x % 4] = 1.0f;
}
