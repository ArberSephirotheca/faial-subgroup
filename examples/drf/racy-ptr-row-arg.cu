__device__ void touch(int *P, int v) { P[0] = v; }

__device__ int *table[4];

__global__ void k(int cat) {
  touch(table[cat], threadIdx.x);
}
