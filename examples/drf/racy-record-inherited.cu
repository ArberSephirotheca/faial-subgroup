// A record whose only member is inherited. A base subobject is laid out
// ahead of the record's own members and its fields are reached without an
// intermediate name, so they are the derived record's fields too.
//
// Collecting only a record's own field declarations left Derived with an
// empty field list, so it was never registered, so the copy could not be
// expanded and named the enclosing object instead.
struct Base { double f[3]; };
struct Derived : Base { };

__global__ void k(Derived *s, Derived *t) {
  if (threadIdx.x == 0) s[0] = t[0];
  if (threadIdx.x == 1) s[0].f[0] = 1.0;
}
