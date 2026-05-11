// Companion to [racy-launch-opaque-block.cu]: same opaque-block
// launch shape ([c.b] of type [dim3], wrapped by cu-to-json into a
// copy-ctor [CXXConstructExpr] that the launch-arg resolver cannot
// decompose), but the kernel writes only via [atomicInc]. Atomic
// accesses are DRF regardless of how many threads contend, so the
// kernel verifies even with all three [blockDim] axes universally
// quantified under [--all-dims]. Pins that the resolver's "no
// constraint" output for opaque axes doesn't accidentally lose DRF
// cases that don't depend on per-axis pinning.
struct Cfg { dim3 b; };

__global__ void drf_opaque_block(unsigned int *cnt) {
    atomicInc(cnt, 1u);
}

void run(unsigned int *cnt) {
    Cfg c;
    c.b = dim3(256);
    drf_opaque_block<<<dim3(64), c.b>>>(cnt);
}
