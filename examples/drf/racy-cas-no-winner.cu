// Same shape as drf-cas-winner.cu but without the [atomicCAS]: the
// plain read of [keys[loc]] gives the analyser no winner contract,
// so two threads with colliding [loc] (via the indirect [hashes]
// lookup) can both pass the [if (old == -1)] gate and the write to
// [values[loc]] is racy. Negative companion to
// drf-cas-winner.cu — pins that atomic-3 fires only on the atomic
// shape and not on any conditional write.
__global__ void k(int n, int *keys, int *values, int *hashes) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    int loc = hashes[idx] % n;
    int old = keys[loc];
    if (old == -1) {
        values[loc] = idx;
    }
}
