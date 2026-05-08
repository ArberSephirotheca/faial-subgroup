// Variadic-template kernel with explicit launches generating two
// specialisations: variadic<int> and variadic<int, int>. The
// resolved template arguments arrive on each specialisation as a
// pack-shaped TemplateArgument whose elements are the individual
// concrete types. The body ignores the pack parameters so the test
// stays focused on the metadata path.
template <typename... Ts>
__global__ void variadic(int *out, Ts...) {
    int tid = threadIdx.x;
    if (tid == 0) out[blockIdx.x] = 0;
}

void run(int *out) {
    variadic<int>      <<<1, 32>>>(out, 1);
    variadic<int, int> <<<1, 32>>>(out, 1, 2);
}
