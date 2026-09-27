// [p->f] is [p[0].f], so a member reached through an arrow carries the
// element index the arrow leaves implicit. Without it the two writes carry
// different index counts, the race check compares only the leading one,
// and cell zero of the member is reported to collide with cell one.
struct Atom { double f[3]; };

__global__ void k(Atom *s) {
  if (threadIdx.x == 0) s->f[0] = 1.0;
  if (threadIdx.x == 1) s[0].f[1] = 2.0;
}
