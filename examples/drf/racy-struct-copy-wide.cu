// A record too wide to list its cells. The copy expands into a loop over
// the cells rather than one access each, so the width stops mattering:
// this is the same shape as racy-struct-copy.cu with a hundred doubles
// instead of three.
//
// Listing the cells needed a bound, and past it the copy kept naming the
// enclosing object, which is a different array from the member and so
// could not meet the member write at all.
struct Wide { double f[100]; };

__global__ void k(Wide *s, Wide *t) {
  if (threadIdx.x == 0) s[0] = t[0];
  if (threadIdx.x == 1) s[0].f[0] = 1.0;
}
