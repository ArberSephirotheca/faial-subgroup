// A subscript in the middle of a member path: [link] is an array of
// objects, so [a[i].link[1].e] indexes it before selecting [e]. That
// subscript belongs to the access's index and not to the region's name,
// and while it stayed in the name the region matched nothing and the
// kernel was discarded.
struct M { float e[3][3]; };
struct S { M link[4]; };

__global__ void k(S *a) {
  a[threadIdx.x % 2].link[1].e[0][0] = threadIdx.x;
}
