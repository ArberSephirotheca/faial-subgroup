# A Modular Static Cost Analysis For GPU Warp-Level Parallelism

## What Is Relational-Cost Analysis?

Relational-cost analysis is a static analysis framework for computing exact symbolic costs of GPU kernels.
It works by establishing a formal relationship between:

1. **GPU kernel semantics** (how threads execute in parallel with divergence)
2. **Sequential program semantics** (a simplified sequential equivalent)
3. **Symbolic cost formulas** (exact expressions for resource usage)

The key insight: GPU kernel costs can be precisely determined by analyzing an equivalent sequential
program using standard cost analysis techniques, rather than reasoning about GPU execution directly.

---

## Overview

GPUs execute threads in groups (warps) in lockstep. The warp size is configurable (typically 32 threads
on NVIDIA GPUs, but this can be adjusted via `lib/config.ml`). Thread divergence selectively disables
threads within a warp. Memory accesses from enabled threads form transactions to memory banks.

**Implementation:**
- **`lib/warp.ml`**: Models warp execution with transaction tracking
  - Type: `TransactionMap.t` groups accesses by memory bank
  - Function: `bank_conflicts()` counts extra cycles from concurrent accesses
  - Function: `uncoalesced()` counts memory transactions from scattered accesses
  - Works with configurable warp size from `config.ml`

- **`lib/transaction.ml`**: Abstracts GPU memory transactions
  - Tracks which threads access same memory bank in same cycle
  - Implements transaction model from paper semantics

---

## Dynamic Cost Model

GPU costs decompose into per-iteration costs. Vectorized expressions represent costs compactly using
vector indices and enabled thread sets. Per-iteration costs are then multiplied by loop iteration
counts to get total costs.

**Implementation:**
- **`lib/vectorized.ml`**: Vectorized expression representation
  - Parameterizes costs over vector dimensions and enabled thread set patterns
  - Enables compact representation of warp-level costs

- **`lib/metric_analysis.ml`**: Generic metric analysis framework
  - Input: Vectorized expression + GPU memory accesses
  - Output: Metric-specific cost (bank conflicts, uncoalesced accesses, etc.)
  - Works by analyzing per-iteration costs, then composing them
  - Maintains exactness information (exact vs. approximate)

- **`lib/config.ml`**: Configuration parameters for metric analysis
  - Memory word size, memory bank count, **warp size** (number of threads per warp)
  - GPU hardware parameters that affect metrics

---

## Static Relational-Cost Analysis

GPU kernel structure maps to sequential program structure. Each GPU cost source maps to a sequential
program annotation. The type system ensures mapping preserves cost semantics.

**Implementation:**
- **`lib/ra_compiler.ml`**: Compiles GPU analysis to Resource Calculus (RA)
  - Input: GPU protocol analysis results
  - Output: Annotated sequential program
  - Process:
    1. Translate GPU loop structure → RA loop structure
    2. Translate GPU memory accesses → RA variable accesses
    3. Translate GPU conditionals → RA conditional costs
    4. Annotate each RA statement with its symbolic cost
    5. Collect exactness statistics

- **`lib/linearize_index.ml`**: Linearizes multi-dimensional array accesses
  - Converts 2D/3D GPU array indices to 1D sequential program indices
  - Preserves cost semantics of access patterns

---

## Meta-Theory And Soundness

Some costs can be proven exactly equal to GPU kernel costs. Others are proven as upper bounds
(approximations). Exactness depends on whether all analysis steps were exact.

**Implementation:**
- **`lib/cost.ml`**: Cost representation with exactness tracking
  ```ocaml
  type t = {
    value : int;           (* The cost value *)
    exact : bool;          (* true = proven exact, false = approximation *)
    state : Transaction.t; (* Link back to GPU execution state *)
  }
  ```

- **`bin/cost.ml`**: Orchestrates exactness tracking throughout pipeline
  - Counters for exact vs. approximate:
    - Index analyses (array access computation)
    - Loop analyses (iteration computation)
    - Condition analyses (branching computation)
  - Final result: exact only if all components are exact

---

## Pico: Cost Analysis For GPU Kernels

The tool orchestrates the entire relational-cost analysis pipeline, integrating GPU semantics analysis
with external symbolic cost solvers.

**Implementation:**
- **`bin/cost.ml`**: Main analysis orchestrator
  - Invokes cost analysis pipeline
  - Integrates external solvers via subprocess calls
  - Supported solvers: Maxima, ABSynth, CoFloCo, KOAT

- **Pipeline:**
  1. Parse GPU kernel → MAP
  2. Analyze MAP → per-iteration vectorized costs
  3. Extract metric → metric-specific per-iteration costs
  4. Compile to RA → annotated sequential program
  5. Send to solver → final symbolic cost formula

- **Additional tools:**
  - **`bin/cost_dyn.ml`**: Dynamic cost computation (executes analysis with concrete values)

---

## Key Code-to-Theory Mapping

| Concept | Implementation |
|---|---|
| **Warp Semantics** | `warp.ml`: Transaction model with enabled thread flags (configurable warp size) |
| **Vectorized Expressions** | `vectorized.ml`: Vector-parameterized expressions for compact cost representation |
| **Metric Analysis** | `metric_analysis.ml`: Parametric framework for extracting specific resource metrics |
| **Bank Conflicts** | `warp.ml::bank_conflicts()`: Groups accesses by bank ID, counts transactions |
| **Uncoalesced Access** | `warp.ml::uncoalesced()`: Groups accesses by address proximity, counts transactions |
| **Relational-Cost** | `ra_compiler.ml`: GPU → RA translation with cost annotations |
| **Cost Exactness** | `cost.ml`: Boolean `exact` flag tracking all approximations |

---

## Using Pico (aka faial-cost)

The binary `faial-cost` can be executed directly via dune as follows.

```bash
# Analyze a GPU kernel for bank conflicts
dune exec bin/cost.exe -m bc kernel.cu

# With external solver (Maxima)
dune exec bin/cost.exe -m bc --maxima kernel.cu

# Specify grid and block dimensions
dune exec bin/cost.exe -m bc --gridDim=64 --blockDim=256 kernel.cu

# Output in JSON format
dune exec bin/cost.exe -m bc --json kernel.cu
```

### Available Metrics

- `bc`: Bank conflicts (shared memory)
- `ua`: Uncoalesced accesses (global memory)
- `count`: Count accesses

### Testing

```bash
# Run unit tests
dune test
```
