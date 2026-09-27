// The same alias with a per-thread element, which reports nothing at all
// when the alias does not resolve.
typedef struct atom_t { double pos[3]; double f[3]; } d_atom;

__global__ void k(d_atom *a) {
  a[threadIdx.x].f[0] = 1.0;
}
