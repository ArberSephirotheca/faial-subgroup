// An object with no members has nothing below it to name, so the object is
// the memory and the copy lands on it. This is the case that keeps the
// rule above from reading as "an object is never memory": a plain [int *]
// is the same shape, one leaf that is the root itself.
struct E { };

__global__ void k(E *s) {
  s[0] = s[1];
}
