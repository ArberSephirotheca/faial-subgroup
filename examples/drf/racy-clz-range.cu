// The shape of drf-clz-range.cu at a stride of 16 instead of 64. The
// 0..32 result range no longer separates the threads, because two
// results can differ by exactly 16, so the kernel stays racy. The pair
// pins that the range is asserted without being stronger than the truth.
__global__ void k(int *out) {
  int t = threadIdx.x;
  out[t * 16 + __clz(t)] = t;
}
