// A host const whose initialiser cu-to-json cannot resolve to a value,
// used both as the block dimension and as a kernel argument. cu-to-json
// inlines the initialiser into every slot that mentions the const, so
// the launch's args carry the call rather than the name, and nothing in
// the launch record says the two came from one variable.
//
// The synth kernel recovers that on its own. The binding is lifted to
// a decl whose initialiser is translated first, which abstracts the
// opaque call behind one launch parameter, and the per-launch dedup
// cache is keyed structurally, so the copy sitting in the arg slot
// resolves to the same parameter:
//
//     __global__ k@... (int * d, int @Launch0) {
//         decl const int n = @Launch0
//         assert(blockDim.x == @Launch0)
//         k(d, @Launch0)
//     }
//
// The verdict rests on that sharing. With blockDim.x equal to the
// argument, threadIdx.x is below it and [threadIdx.x % n] is the
// identity, so every thread writes its own cell. Give the argument its
// own binding and the analysis is right to report a race: see
// racy-launch-unrelated-const-arg.cu, which differs only in that.
int read_size();

__global__ void k(int *a, int n) { a[threadIdx.x % n] = threadIdx.x; }

void run(int *d) {
  const int n = read_size();
  k<<<1, n>>>(d, n);
}
