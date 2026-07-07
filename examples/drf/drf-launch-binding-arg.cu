struct slot_info { int slot; int token; };
static slot_info acquire();

__global__ void k(int * d, int s) { d[threadIdx.x] = s; }

void launch(int * d) {
    const auto [slot, token] = acquire();
    k<<<1, 256>>>(d, slot);
}
