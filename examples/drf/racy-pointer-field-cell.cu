// The same bucket reached twice, once through the arrow and once through
// an explicit zero. A zero subscript is elided when the region is named,
// which is an identity C already supplies since p[0] and *p denote the
// same location, so both writes land on the same region and race.
struct Bucket { int *items; };

__global__ void k(Bucket *b) {
  if (threadIdx.x == 0) b->items[0] = 1;
  if (threadIdx.x == 1) b[0].items[0] = 2;
}
