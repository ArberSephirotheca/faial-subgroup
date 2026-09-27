// The same through a method returning its object's pointer member. The
// member is read for its value here rather than subscripted, and the
// region a pointer member names is what has to come back.
struct Acc { float *p; __device__ float *data() { return p; } };

__global__ void k(Acc a) { a.data()[threadIdx.x] = threadIdx.x; }
