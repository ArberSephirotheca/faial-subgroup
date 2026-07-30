// The mirror of racy-deref-address-of.cu: [&*(a + 0)] is the address of
// a[0], not a read of it. Left uncancelled, the deref underneath matches
// the read shape and the kernel gains an access it never performs, which
// collides with the store and reports a race that is not there.
__global__ void k(int *a, long *out) {
  if (threadIdx.x == 0) a[0] = 1;
  if (threadIdx.x == 1) out[1] = (long)&*(a + 0);
}
