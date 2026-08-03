// Exercises IntegerLiteral parsing for uint64 sentinels that don't fit
// OCaml's 63-bit native int. 0xFFFFFFFFFFFFFFFFULL parses through the
// "0u"-prefixed Int64 path (unsigned reinterpretation as signed -1);
// 0xFF42E54B94E2DA0DULL is a real-world sentinel observed in
// HeCBench's logic-rewrite-cuda. Both must reach the analyzer as
// concrete integers, not as the Int.max_int fallback.
#define EMPTY ((unsigned long long)0xFFFFFFFFFFFFFFFFULL)
#define MAGIC ((unsigned long long)0xFF42E54B94E2DA0DULL)

__global__ void uint64_sentinel(unsigned long long *out, int n) {
  __assume(blockDim.y == 1 && blockDim.z == 1);
  __assume(gridDim.y == 1 && gridDim.z == 1);
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n) {
    unsigned long long v = out[i];
    out[i] = (v == EMPTY) ? MAGIC : v;
  }
}
