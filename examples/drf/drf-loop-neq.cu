__global__
void neq_plain(int *out, int n)
{
  if (threadIdx.x == 0) {
    for (int i = 0; i != n; i++) { out[i] = 1; }
  } else if (threadIdx.x == 1) {
    out[n] = 2;
  }
}

__global__
void neq_flipped(int *out, int n)
{
  if (threadIdx.x == 0) {
    for (int i = 0; n != i; i++) { out[i] = 1; }
  } else if (threadIdx.x == 1) {
    out[n] = 2;
  }
}

__global__
void neq_sub(int *out, int n)
{
  if (threadIdx.x == 0) {
    for (int i = 0; i - n != 0; i++) { out[i] = 1; }
  } else if (threadIdx.x == 1) {
    out[n] = 2;
  }
}

__global__
void neq_sub_flipped(int *out, int n)
{
  if (threadIdx.x == 0) {
    for (int i = 0; 0 != i - n; i++) { out[i] = 1; }
  } else if (threadIdx.x == 1) {
    out[n] = 2;
  }
}

__global__
void neq_bare(int *out, int n)
{
  if (threadIdx.x == 0) {
    for (int i = 0; i - n; i++) { out[i] = 1; }
  } else if (threadIdx.x == 1) {
    out[n] = 2;
  }
}

__global__
void neq_bounded(int *out, int n)
{
  if (threadIdx.x == 0) {
    for (int i = 0; i != n; i++) { out[i] = 1; }
  } else if (threadIdx.x == 1) {
    out[n + 1] = 2;
  }
}

__global__
void neq_down(int *out, int n)
{
  if (threadIdx.x == 0) {
    for (int i = n; i != 0; i--) { out[i] = 1; }
  } else if (threadIdx.x == 1) {
    out[0] = 2;
  }
}
