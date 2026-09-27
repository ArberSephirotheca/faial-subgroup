// A record and the kernel that uses it in one namespace. A type is spelled
// as it is written where it is used, so from inside the namespace the
// parameter says Cut while the declaration registers rw::Cut, and comparing
// the two spellings as strings found nothing.
namespace rw {

struct Cut { int sign, leaves[4]; };

__global__ void k(int id, Cut *cuts) {
  cuts[id].leaves[threadIdx.x % 4] = threadIdx.x;
}

}
