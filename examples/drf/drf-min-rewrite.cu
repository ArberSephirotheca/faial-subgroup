// [min] has a body rather than a declaration: it lowers to a
// conditional over its two arguments, so the solver sees min's actual
// graph. Here min(t, 3) lies in 0..3 and a stride of 4 keeps the
// writes apart. Leaving min an uninterpreted function, which is what
// happens when the body only fires on literal arguments, makes the
// result an unbounded integer and reports a race.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 4 + min(t, 3)] = t;
}
