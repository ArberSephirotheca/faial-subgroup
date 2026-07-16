# ggml-cuda Subgroup Alarm Investigation

## Scope

This note reviews the remaining `not_drf` results from the 31-kernel subgroup
campaign in:

```text
/Users/zheyuan/GPU-DRF/faial-comparison-logs/
  subgroup-semantic-uniformity-final-20260715
```

The campaign contains 13 `not_drf` rows:

- 12 `topk_moe_cuda` specializations, each with one SAT `ids` write/write
  obligation.
- 1 `mm_ids_helper<2>` specialization with four SAT obligations.

The review distinguishes three claims:

1. Whether the exact solver obligation has a production-valid model.
2. Whether the corresponding source access pair is genuinely racy under the
   subgroup DRF semantic target.
3. Which additional application invariant would exclude the race.

This distinction matters for `mm_ids_helper`: one emitted model uses an
impossible launch shape. Under the subgroup DRF model, the intervening fully
convergent warp primitives order same-subgroup accesses, so the production
one-warp launch discharges that alarm. Strict CUDA-spec memory ordering is a
separate portability boundary discussed below; it is not the semantic target
of this classification.

## Summary

| Alarm set | Exact solver result | Source classification | Required condition or fix |
| --- | --- | --- | --- |
| 12 `topk_moe_cuda` `ids` write/write alarms | SAT | True when a biased selection score is NaN; false when every selection score is non-NaN | Sanitize `selection_wt` after adding bias, or enforce a non-NaN input contract |
| `mm_ids_helper` obligation 0, shared write/write | SAT | True when one token selects the same expert more than once | Enforce pairwise-distinct expert IDs per token, or make duplicate handling race-free |
| `mm_ids_helper` obligation 1, shared write/read | SAT only through an unresolved launch dimension | False alarm under the subgroup DRF model: the production launch contains one subgroup and intervening warp primitives order its accesses | Preserve `blockDim.x = subgroup_size` in the launch contract |
| `mm_ids_helper` obligations 2 and 3, output write/write | SAT | False alarm | Model the lane-uniform result of `warp_reduce_sum` |

At row granularity, the 12 `topk_moe_cuda` rows have a concrete race under the
low-level input contract. The `mm_ids_helper` row has a concrete race only
when duplicate expert IDs are admitted; under the normal pairwise-unique
producer contract, all four reported `mm_ids_helper` obligations are false
under the subgroup DRF model.

## `topk_moe_cuda`

### Affected rows

The 12 launch rows cover:

```text
topk_moe_280  n_experts =   1
topk_moe_284  n_experts =   2
topk_moe_288  n_experts =   4
topk_moe_292  n_experts =   8
topk_moe_296  n_experts =  16
topk_moe_300  n_experts =  32
topk_moe_304  n_experts =  64
topk_moe_308  n_experts = 128
topk_moe_312  n_experts = 256
topk_moe_316  n_experts = 288
topk_moe_320  n_experts = 512
topk_moe_324  n_experts = 576
```

The extracted subgroup sites are the biased branch at
`topk-moe.cu:185-187`. Every alarm is the write at `topk-moe.cu:228`:

```cpp
if ((max_expert & (WARP_SIZE - 1)) == threadIdx.x) {
    ids[k] = max_expert;
}
```

Faial currently preserves the writer guard but does not model the value
relation established by the XOR argmax reduction. It therefore allows
`max_expert$T1` and `max_expert$T2` to differ independently.

### Concrete true counterexample

Use the normal launch:

```text
blockDim = (32, 4, 1)
blockIdx = (0, 0, 0)
threadIdx.y = 0
n_rows >= 1
n_expert_used >= 1
k = 0
has_bias = true
```

Choose post-bias selection scores as follows:

```text
selection_wt[expert 0] = NaN
selection_wt[expert 1] = 1.0       when n_experts > 1
all other valid experts = 0.0
invalid lanes = -infinity
```

The same failure also applies to `n_experts = 1`: lane 1 retains its invalid
`-infinity` candidate with expert id 1.

CUDA floating-point comparisons with NaN make both `>` and `==` false.
Consequently:

- Lane 0 never replaces `(NaN, expert 0)`.
- Lane 1 and the other non-NaN lanes reduce to expert 1.
- Lane 0 satisfies the writer guard because `0 % 32 == 0`.
- Lane 1 satisfies the writer guard because `1 % 32 == 1`.
- Both lanes write `ids[0]`, with values 0 and 1.

This counterexample works for all 12 `n_experts` specializations. The source
sanitizes NaNs in `wt` at lines 123-133, but the biased path creates
`selection_wt = wt + bias` at lines 139-149 and does not sanitize the result.
A NaN bias is sufficient; `+infinity + -infinity` can also create a NaN.

No source or launch precondition requires the bias or the resulting selection
score to be non-NaN. The alarm is therefore true under the kernel's current
input contract.

### Proof under a non-NaN guard

Assume every candidate selection score is non-NaN. Define a strict total order
on candidate pairs:

```text
(score_a, expert_a) >argmax (score_b, expert_b)

iff

score_a > score_b
or
(score_a = score_b and expert_a < expert_b)
```

The local per-lane loop selects the maximum pair held by that lane. For equal
scores it retains the first candidate, which is also the smaller expert id.

Each XOR step combines two candidate sets using the same total-order maximum.
Maximum under a total order is associative, commutative, and idempotent.
After masks `16, 8, 4, 2, 1`, every lane therefore holds the same global pair
`(score, E)`.

Exactly one lane satisfies:

```text
threadIdx.x = E mod 32
```

so exactly one lane writes `ids[k]`. The selected lane changes its private
candidate to `-infinity`; induction gives the same property for every later
`k`.

Therefore:

- The no-bias path is DRF for this access because `wt` is sanitized first.
- The bias path is DRF only under the guard that every post-addition
  `selection_wt` value is non-NaN.

The direct source fix is to sanitize `selection_wt` after `wt + bias`.

## `mm_ids_helper<2>`

The production launch uses:

```text
gridDim.x = n_experts
blockDim.x = device physical warp size
```

For the CUDA target used by the campaign:

```text
blockDim = (32, 1, 1)
subgroup_size = 32
```

The extracted goal retains `blockDim.x = @Launch6` with only
`@Launch6 > 0`. This missing equality explains part, but not all, of the
reported behavior.

### Obligation 0: shared write/write

The conflicting source access is `mmid.cu:88`:

```cpp
store[it_compact + it_compact_add_lower] =
    mm_ids_helper_store(it, iex_used);
```

Use this valid launch and input:

```text
n_experts = 2
n_tokens = 1
n_expert_used = 2
blockIdx.x = 0
blockDim = (32, 1, 1)
ids for token 0 = [0, 0]
```

For expert block 0:

| Lane | `it` | `iex` | `expert_used` | `iex_used` | target index | stored value |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 0 | 0 | 0 | 0 | 0 | 0 | 0 |
| 1 | 0 | 1 | 0 | 1 | 0 | 4194304 |

Both lanes execute the same non-atomic shared-memory write to `store[0]`.
This is a concrete write/write race.

Normal llama.cpp MoE routing obtains selected expert IDs from
`ggml_argsort_top_k`, and the backend tests deliberately generate unique IDs
per token. Under pairwise uniqueness, at most one lane in each token group can
have `iex_used != -1`.

Pairwise uniqueness is not part of the public `ggml_mul_mat_id` checks in
`ggml.c:3300-3323`, however. The low-level alarm is therefore true, while the
usual `argsort_top_k` producer excludes this particular witness.

Under the uniqueness guard, shared write indices are injective:

- `it_compact_add_lower` is the number of earlier token groups that use the
  current expert.
- Two active token groups have different prefix ranks.
- `it_compact` advances by the number of active groups in each prior loop
  chunk.
- Consequently, active groups in one chunk and across different chunks write
  disjoint shared indices.

### Obligation 1: shared write/read under the subgroup model

The write is at line 88 and the read is at lines 97-98:

```cpp
for (int itc = threadIdx.x; itc < it_compact; itc += warp_size) {
    const mm_ids_helper_store store_it = store[itc];
}
```

The emitted Faial obligation contains:

```text
threadIdx.x$T1 / 32 != threadIdx.x$T2 / 32
```

That requirement comes from the current rule that treats intervening shuffle
and reduction operations as subgroup memory-ordering boundaries. Combined
with the unresolved `@Launch6`, Z3 can invent a block larger than one warp.
That exact witness is impossible for the production launch.

In the subgroup DRF model, every recognized fully convergent warp primitive
acts as a subgroup-local synchronization and ordering boundary. The production
launch has exactly one subgroup:

```text
blockDim = (32, 1, 1)
subgroup_size = 32
```

There is therefore no pair of production threads satisfying the obligation's
different-subgroup clause. This obligation is a false alarm caused by losing
the equality between the runtime physical warp size and the configured
subgroup size.

#### Strict CUDA portability boundary

The subgroup DRF model intentionally gives fully convergent warp primitives
barrier-like ordering. That is a model assumption, not the literal memory
ordering specified for CUDA shuffle intrinsics. CUDA documents
`__shfl*_sync` as value-exchange operations without a general memory-ordering
guarantee, while `__syncwarp()` provides warp-local memory ordering.

If the verification claim is changed from the subgroup DRF model to the strict
CUDA language memory model, the shared write/read should be protected by
`__syncwarp()`. That is a portability recommendation, not a true alarm under
the current subgroup model.

### Obligations 2 and 3: output write/write

The writes are:

```cpp
ids_src1[nex_prev + itc] = ...;
ids_dst [nex_prev + itc] = ...;
```

Faial treats the result of `warp_reduce_sum<warp_size>(nex_prev)` as unrelated
per-thread symbolic values. The goals therefore contain independent
`nex_prev$T1` and `nex_prev$T2`.

The helper semantics instead gives every participating lane the same reduced
value `N`. Each lane writes indices of the form:

```text
N + lane + q * W
```

where `W` is the physical warp size and `0 <= lane < W`.

Suppose two distinct lanes write the same index:

```text
N + lane_1 + q_1 * W = N + lane_2 + q_2 * W
```

Taking both sides modulo `W` gives:

```text
lane_1 = lane_2
```

which contradicts distinct threads. These two obligations are false alarms.
The required analyzer improvement is a value relation for full-subgroup
reductions: all participating lanes receive the same result.

## Validation Evidence

A lane-level simulator reproduced the `topk_moe` NaN counterexample for all
12 expert counts:

```text
final experts = {0, 1}
writer lanes = {0, 1}
```

It also checked 12,000 randomized non-NaN score vectors; every run produced
one common expert and one writer. The algebraic total-order argument above is
the proof; the randomized run is only a regression check.

For `mm_ids_helper<2>`, a lane-level simulation produced:

```text
duplicate ids [0, 0]:
  lane 0 writes store[0] = 0
  lane 1 writes store[0] = 4194304

unique ids [0, 1]:
  lane 0 alone writes store[0]
```

It also checked 18,144 random unique-ID token chunks with no shared
write/write collision. Again, the prefix-rank argument is the proof.

### Ordinary-pipeline parity oracle

The author-reported `group_norm_f32@norm_300` alarm is also used as a
regression for the boundary between ordinary and subgroup routing. Its
`block_reduce` helper writes `s_sum[1]` from lane 32 at `common.cuh:629` and
reads `s_sum[1]` from lane 1 at `common.cuh:634`, with `group_size = 1024`.

The exact production command reports the same `racy` verdict and the same
counterexample both with and without `--subgroup-size 32`. This parity matters
because the race is between different warps and is part of the original MAP
memory analysis. A subgroup-aware invocation must preserve the full-program
ordinary lowering and all device-helper memory effects; subgroup routing is
not allowed to replace that path with an isolated-kernel translation.

## Required Follow-up

1. Preserve the exact `mm_ids_helper` launch relation.
   Record `blockDim.x = physical_warp_size = subgroup_size` so the invalid
   cross-subgroup witness is unavailable.
2. Add collective result relations.
   Full-subgroup argmax and reduction helpers need guarded value summaries so
   valid equal-result invariants can discharge false alarms.
3. Fix or constrain `topk_moe_cuda`.
   Sanitizing post-bias selection scores is the direct robust fix.
4. Define the `mm_ids_helper` input contract.
   Decide whether duplicate expert IDs are rejected or supported without
   races.
5. Keep the CUDA portability boundary explicit.
   A strict CUDA-spec mode would require `__syncwarp()` for the shared-memory
   handoff; the current subgroup model intentionally treats the intervening
   warp primitives as ordering boundaries.

## External Semantic Reference

The CUDA C++ Programming Guide states that warp shuffle intrinsics do not
guarantee memory ordering, while `__syncwarp()` does provide memory ordering
among participating warp threads:

- <https://docs.nvidia.com/cuda/archive/13.0.1/cuda-c-programming-guide/index.html#warp-shuffle-functions>
- <https://docs.nvidia.com/cuda/archive/13.0.1/cuda-c-programming-guide/index.html#warp-synchronization-function>
