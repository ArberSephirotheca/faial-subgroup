// Storing a whole record touches every scalar under it, so the store to
// [s[0]] and the store to [s->f[0]] land on the same cell. Before the
// store was expanded into one access per leaf it named the enclosing
// element instead, which is a different array from [s.f], and two writes
// to the same bytes were reported race-free.
struct Atom { double f[3]; };

__global__ void k(Atom *s, Atom *t) {
  if (threadIdx.x == 0)      s->f[0] = 1.0;
  else if (threadIdx.x == 1) s[0] = t[0];
}
