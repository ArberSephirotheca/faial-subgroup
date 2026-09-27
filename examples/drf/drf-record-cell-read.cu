// The twin of racy-record-cell-read.cu with each thread on its own site,
// so the members the read expands into are apart from the ones the write
// expands into.
struct C { float re, im; };
struct M { C e[2][2]; };
struct S { M link[2]; };

__global__ void k(S *a) {
  C v = a[threadIdx.x].link[0].e[0][0];
  a[threadIdx.x].link[1].e[0][0] = v;
}
