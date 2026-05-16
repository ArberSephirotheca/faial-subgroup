// Each thread writes to a unique cell of arr — semantically race-free.
// The callee [f] has a local [int i;] whose name collides with the
// caller's [i]. After our inliner runs, faial-drf's map for [k] is:
//
//   if ((0 *u 1024 +u threadIdx.x) < n) {
//     int i1;
//     rw arr[0 + i1]
//   }
//
// [f]'s local [i] was alpha-renamed to [i1], but the parameter
// substitution that should have replaced [p] with the call-site
// expression [arr + i] (caller's [i] = blockIdx*blockDim + threadIdx)
// resolved [i] against the renamed local [i1] instead of the caller's
// binding. Each thread's write index then reads [i1] (uninitialised
// callee-local) and the analysis concludes [arr[0] = ...] for every
// thread.
//
// Minimal trigger: removing the local [int i;] from [f] clears the
// kernel as DRF; renaming the local to a non-colliding name (e.g.
// [int j;]) also clears it; using a literal on the RHS [*p = 42]
// masks the race as a benign same-value write but does not fix the
// substitution. The local need not be read or initialised.

__device__ void f(int *p, int v) {
  int i;
  *p = v;
}

__global__ void k(int *arr, int n) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n)
    f(arr + i, 42);
}
