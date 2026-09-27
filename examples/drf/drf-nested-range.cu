// A declaration is instantiated once per occurring application, and
// an application nested inside another is still one of them. The
// write index needs __ffs's range and __clz's range together: the sum
// lies in 0..64 and a stride of 128 keeps the writes apart. Dropping
// either range reports a race.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 128 + __ffs(t) + __clz(__ffs(t))] = t;
}
