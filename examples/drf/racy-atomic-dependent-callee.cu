// An atomic in a template that is never instantiated. With the argument
// type dependent the overload set is still open, so clang names the
// callee as an unresolved lookup rather than as a declaration, and a
// guard that accepts only the resolved spelling leaves the builtin
// standing as a call with no body.
template <typename T>
__global__ void k(T *c) {
  if (threadIdx.x == 0) atomicAdd(c, 1);
  if (threadIdx.x == 1) c[0] = 1;
}
