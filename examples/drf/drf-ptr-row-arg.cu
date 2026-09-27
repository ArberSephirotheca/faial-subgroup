__device__ void touch(int *P, int i, int v) { P[i] = v; }

__device__ int *table[4];

__global__ void k(int cat) {
  touch(table[cat], threadIdx.x, threadIdx.x);
}
