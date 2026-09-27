// A record declared with the typedef idiom, which is how a C-style header
// names an aggregate. The record registers under its own tag while every
// use of it says the alias, so nothing bridged the two names and every
// member access named a variable in no array map.
//
// A typedef of a record is kept only when it renames one. The self-named
// form, typedef struct X { ... } X, is how CUDA declares its vector types,
// whose lanes are handled apart from records.
typedef struct atom_t { double pos[3]; double f[3]; } d_atom;

__global__ void k(d_atom *a) {
  a[0].f[threadIdx.x % 3] = 1.0;
}
