// The vectorised store as CUDA writes it: take the address of an
// element, widen the pointer, and store through it. The cast is deleted
// before the C-AST is built, so what reaches the D-lowering matcher is
// [*&a[0] = v]. Cancelling the pair leaves [a[0] = v], the store every
// thread performs, and without the cancellation nothing matches and the
// kernel is reported as having no accesses at all.
//
// The footprint is still one element: the store covers a[0] and a[1],
// and the width the cast carried is gone by the time the access is
// minted. See racy-ptr-view-subscript.cu.
struct __align__(8) Pair { int x, y; };

__global__ void k(int *a) {
  *reinterpret_cast<Pair *>(&a[0]) = Pair{(int)threadIdx.x, 0};
}
