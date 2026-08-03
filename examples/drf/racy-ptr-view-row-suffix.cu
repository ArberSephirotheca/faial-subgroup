// A view rooted part-way into an array, which is the shape a row taken out
// of a table has. One axis is already fixed by the subscript on the way in,
// so the flat index splits across the dimensions below that subscript and
// the subscript stays in front: row[5] is B[i][1][1], which is where the
// direct write lands.
__global__ void k(int i) {
  __shared__ int B[2][4][4];
  int *row = (int *)B[i];
  if (threadIdx.x == 0) row[5] = 1;
  if (threadIdx.x == 1) B[i][1][1] = 7;
}
