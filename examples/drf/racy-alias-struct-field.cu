// A pointer bound to a struct and a member reached through it: the binder
// names the object while the access names the member, so resolution has to
// match the access's root against the binder rather than its whole name.
// Before it did, the access named a variable in no array map and was
// deleted, and the kernel reported no accesses at all.
struct Atom { double f[3]; };

__global__ void k(Atom *s) {
  Atom *q = s;
  q->f[threadIdx.x % 3] = 1.0;
}
