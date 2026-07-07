__global__ void k(int * d)
{
  auto store = [&](auto v){ d[v] = v; };
  store((int)threadIdx.x);
}
