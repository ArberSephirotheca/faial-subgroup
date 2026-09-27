// A member whose extent the type does not state. The loop runs to a bound
// that is an uninterpreted function of the object, so two threads asking
// about the same object get the same bound and the solver chooses one
// where the copy reaches cell zero.
//
// Listing the cells needed a count, so an unstated extent declined
// outright and the copy kept naming the enclosing object.
struct Flex { int n; double f[]; };

__global__ void k(Flex *s, Flex *t) {
  if (threadIdx.x == 0) s[0] = t[0];
  if (threadIdx.x == 1) s[0].f[0] = 1.0;
}
