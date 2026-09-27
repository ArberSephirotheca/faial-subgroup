// A callable passed to a kernel as a by-value template parameter, which is
// how an elementwise launcher takes its body. The lambda is written in host
// code, so the only copy of the body sat inside a function faial dropped
// whole, and the kernel had no memory access at all.
//
// The closure's fields are unnamed and its body reads its captures as free
// variables, so each field takes the name of the capture that fills it and
// the body reads them through the object.
template <typename F>
__global__ void run(int n, F f) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < n) f(i);
}

template <typename F>
static void launch(int n, F f) {
  run<<<1, 32>>>(n, f);
}

static void host(int *out, int n) {
  auto body = [=] __device__(int i) { out[0] = i; };
  launch(n, body);
}

int main() {
  int *out;
  cudaMalloc((void **)&out, 4);
  host(out, 32);
  return 0;
}
