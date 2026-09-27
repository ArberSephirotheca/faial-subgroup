namespace N { __device__ void touch(int *A, int i); }

__device__ void N::touch(int *A, int i) {
  A[i] = i;
}
