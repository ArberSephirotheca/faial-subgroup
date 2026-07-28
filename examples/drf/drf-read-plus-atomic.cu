// Reads plus one atomic increment. Every thread reads the same two
// cells and atomically adds into a third, so no two accesses ever
// conflict and the race proof is trivial. The protocol still holds
// three accesses, so the verdict stays drf: a trivial proof is not the
// same condition as an empty protocol, and the zero-accesses status
// must not fire here.
__global__ void k(const int *x, int *counter) {
  int a = x[0];
  int b = x[1];
  atomicAdd(counter, a + b);
}
