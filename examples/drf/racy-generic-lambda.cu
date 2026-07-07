__global__ void k(int * d)
{
  auto store = [&](auto v){ d[0] = v; };
  store((int)threadIdx.x);
}
