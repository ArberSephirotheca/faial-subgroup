// The same callable, bound in the function that launches rather than one
// frame above it. The launch wrapper binds every host constant it captured
// by rewriting its initialiser, and here that initialiser is the lambda, so
// rewriting it put the lambda in an expression position and aborted the run.
// A closure has no value to bind: its type is what identifies it.
template <typename F>
__global__ void run(int n, F f) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n) f(i);
}

static void launch(int *out, int n) {
  auto body = [=] __device__(int i) { out[0] = i; };
  run<<<1, 32>>>(n, body);
}

int main() {
  int *out;
  cudaMalloc((void **)&out, 4);
  launch(out, 32);
  return 0;
}
