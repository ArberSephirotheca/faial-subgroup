// The same shape with a per-thread element, which reports nothing at all
// when the unqualified name does not resolve.
namespace rw {

struct Cut { int sign, leaves[4]; };

__global__ void k(Cut *cuts) {
  cuts[threadIdx.x].leaves[0] = 1;
}

}
