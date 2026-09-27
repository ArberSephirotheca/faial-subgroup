// Two records of the same bare name, one at global scope and one in a
// namespace. Keying on the bare name let the later declaration overwrite
// the earlier, so [S] resolved to the members of [a::S] and the access on
// [f] named a variable in no array map and was deleted.
struct S { float f[4]; };
namespace a { struct S { int *p; }; }

__global__ void k(S *y) {
  y->f[threadIdx.x % 4] = 1.0f;
}
