// The same [const] parameter with two members, so the failure shifts the
// arguments rather than merely dropping one: the callee holds two
// parameters against three arguments, [i] takes [w.q], and the racy write
// to [v.q[0]] is lost. The write to [B] survives, so the kernel is not
// empty and the loss reads as a race-free verdict rather than as a dropped
// access.
typedef struct V { int *p; int *q; } V;

__device__ void put(const V v, int i) { v.q[0] = i; }

__global__ void k(V w, int *B) {
  B[threadIdx.x] = 1;
  put(w, threadIdx.x);
}
