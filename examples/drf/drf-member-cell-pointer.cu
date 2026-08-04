// Two addresses held in two cells that are spelled at different depths:
// one subscript indexes the object, the other indexes the array member.
// Both cells are decided statically, so each names a region of its own.
// Folding every subscript onto the root gave both writes the same name,
// and the kernel reported a race that cannot happen unless the two cells
// hold one address.
struct In  { int *p; };
struct Mid { In a[2]; };

__global__ void k(Mid *s) {
  if (threadIdx.x == 0) s[1].a[0].p[0] = 1;
  if (threadIdx.x == 1) s[0].a[1].p[0] = 2;
}
