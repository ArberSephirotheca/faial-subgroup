// A record declared in a namespace. Every use of a record type is spelled
// in full while the declaration carries only the bare name, so keying the
// declaration on the bare name left [a::S] matching nothing: the member
// arrays were never derived and the kernel reported no accesses at all.
namespace a { struct S { float f[4]; }; }

__global__ void k(a::S *x) {
  x->f[threadIdx.x % 4] = 1.0f;
}
