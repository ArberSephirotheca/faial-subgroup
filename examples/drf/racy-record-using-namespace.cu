// The same spelling reached the other way: a using-directive makes the name
// visible unqualified from outside the namespace, so the kernel's parameter
// says Cut while the declaration registers rw::Cut.
namespace rw { struct Cut { int sign, leaves[4]; }; }
using namespace rw;

__global__ void k(int id, Cut *cuts) {
  cuts[id].leaves[threadIdx.x % 4] = threadIdx.x;
}
