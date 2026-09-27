// Reading a cell that holds a record reads every member of it, and the
// cell is reached through a member path with subscripts of its own. The
// type says how many of those subscripts it takes, which is the ones
// written below the member and not the whole path's, and counting the
// whole path left the read unexpanded and unnamed. Here the cell one
// thread reads is the cell another writes.
struct C { float re, im; };
struct M { C e[2][2]; };
struct S { M link[2]; };

__global__ void k(S *a) {
  C v = a[1].link[0].e[0][0];
  a[threadIdx.x].link[0].e[0][0] = v;
}
