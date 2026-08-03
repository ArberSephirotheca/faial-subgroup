// The same inherited member with every thread on its own element, and a
// member the derived record declares itself, so both halves of the field
// list are exercised.
struct Base { double f[3]; };
struct Derived : Base { int n; };

__global__ void k(Derived *s, Derived *t) {
  s[threadIdx.x] = t[threadIdx.x];
  s[threadIdx.x].f[0] = 1.0;
}
