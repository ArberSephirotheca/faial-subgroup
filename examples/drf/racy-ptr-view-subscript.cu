// The expression form of a pointer view, where the cast sits at the
// subscript rather than at a declaration. Nothing downstream can see it:
// the parser deletes a cast whose sides are not both integer scalars, so
// ((F4 *)A)[0] reaches the D-AST as A[0] with no trace of the width. The
// store covers elements 0 to 3 and collides with the direct write to
// element 3, and faial reports the kernel data-race free.
//
// Recovering it means keeping pointer casts in the conversion
// representation, which is why this shape is documented here rather than
// checked.
struct F4 { float x, y, z, w; };

__global__ void k(float *A, F4 v) {
  if (threadIdx.x == 0) ((F4 *)A)[0] = v;
  if (threadIdx.x == 1) A[3] = 7.0f;
}
