// An object whose members are the memory is not memory itself, so a copy
// of the whole object has to reach the members to say anything. This one
// cannot: its only member has no cells, so the expansion declines and the
// copy names the object. Registering the object too would let the copy be
// analyzed under a name that denotes the very bytes its members do, which
// is the merge the addressing model exists to rule out.
struct Z { int a[0]; };

__global__ void k(Z *s) {
  s[0] = s[1];
}
