__global__ void k(int *A, unsigned n) {
  auto store = [&](auto v){ A[v] = threadIdx.x; };
  store((int)threadIdx.x);
  store(n);
}
