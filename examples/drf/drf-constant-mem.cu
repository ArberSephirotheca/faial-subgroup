__constant__ float t[4];

__global__ void k(int i) { float x = t[i]; }
