# Architecture: A Modular Static Cost Analysis For GPU Warp-Level Parallelism

## Overview

The `rel_cost` module implements a **relational-cost analysis** framework for GPU kernels.
It establishes a formal relationship between:
- **GPU kernel semantics**: How threads execute in lockstep on GPU hardware
- **Sequential program semantics**: A simplified sequential representation
- **Symbolic cost expressions**: Exact formulas describing resource usage

This enables precise analysis of GPU kernel costs (bank conflicts, uncoalesced memory accesses, etc.)
by translating GPU execution into a sequential program and analyzing it with standard cost analysis tools.

---

## Dataflow Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│  GPU KERNEL ANALYSIS PHASE                                      │
└─────────────────────────────────────────────────────────────────┘

Input: GPU Kernel Code (.cu file) + Specification (.thm file)
   │
   ├─→ [Protocol Analysis]
   │   └─→ lib/protocol_analysis.ml
   │       ├─ Analyzes GPU kernel structure
   │       ├─ Determines enabled/disabled threads per iteration
   │       └─ Computes vectorized expressions
   │
   └─→ [Warp Semantics & Transactions]
       └─→ lib/warp.ml + lib/transaction.ml
           ├─ Simulates warp-level execution (32 threads in lockstep)
           ├─ Tracks which threads are enabled (not diverged)
           ├─ Groups memory accesses into transactions
           └─ Computes per-iteration costs


┌─────────────────────────────────────────────────────────────────┐
│  METRIC ANALYSIS PHASE                                          │
└─────────────────────────────────────────────────────────────────┘

   ├─→ [Metric Analysis]
       └─→ lib/metric_analysis.ml
           ├─ Bank Conflict Metric:
           │  └─ How many extra cycles due to multiple threads
           │     accessing same memory bank?
           │
           ├─ Uncoalesced Access Metric:
           │  └─ How many memory transactions needed for sequential access?
           │
           └─ Generic Parametric Analysis:
              └─ Framework for analyzing arbitrary GPU metrics


┌─────────────────────────────────────────────────────────────────┐
│  COST REPRESENTATION PHASE                                      │
└─────────────────────────────────────────────────────────────────┘

   ├─→ [Cost Model]
   │   └─→ lib/cost.ml
   │       ├─ Cost = { value: int, exact: bool, state: Transaction option }
   │       ├─ 'exact' flag: marks if cost is exact or approximate
   │       └─ Transaction state: enables mapping back to GPU semantics
   │
   └─→ [Vectorized Expressions]
       └─→ lib/vectorized.ml
           ├─ Represents per-iteration costs compactly
           └─ Handles vector operations across thread groups


┌─────────────────────────────────────────────────────────────────┐
│  RESOURCE CALCULUS COMPILATION PHASE                            │
└─────────────────────────────────────────────────────────────────┘

   ├─→ [RA Compilation]
       └─→ lib/ra_compiler.ml
           ├─ Translates GPU analysis into Resource Calculus (RA)
           ├─ RA: Sequential program with symbolic cost annotations
           ├─ Per-iteration costs → loop body costs
           ├─ Collects: indices, loops, conditionals with exactness info
           └─ Output: Annotated sequential RA program


┌─────────────────────────────────────────────────────────────────┐
│  SYMBOLIC COST SOLVING PHASE                                    │
└─────────────────────────────────────────────────────────────────┘

   └─→ [External Solver Integration]
       ├─ bin/cost.ml (Pico)
       │  └─ Invokes external solvers with compiled RA program
       │
       ├─ Solvers: Maxima, ABSynth, CoFloCo, KOAT
       │  ├─ Take annotated RA program
       │  ├─ Apply loop analysis techniques
       │  └─ Return symbolic cost formula
       │
       └─→ [Output]
           ├─ Symbolic formula: e.g., "2*n + m + 5"
           ├─ Exactness classification:
           │  ├─ Exact: all indices, loops, conditions computed exactly
           │  ├─ Approximate: some components overestimated
           │  └─ Breakdown by component (indices/loops/conditions)
           │
           └─ Statistics: analysis time, approximation counts
```

---

## Module Dependency Graph

```
bin/
├── cost.ml (Pico)
│   └── Implements the static relational-cost analysis
│       Combines: protocol analysis → metric analysis
│                 → RA compilation → external solvers
│
└── cost_dyn.ml
    └── Dynamic analysis tool (executes analysis with concrete values)

lib/
├── [CORE SEMANTICS]
│   ├── warp.ml
│   │   └── Warp execution semantics (lockstep, transactions)
│   │
│   ├── vectorized.ml
│   │   └── Vectorized expression representation
│   │
│   └── transaction.ml
│       └── GPU memory transaction abstraction
│
├── [ANALYSIS FRAMEWORKS]
│   ├── protocol_analysis.ml
│   │   └── Analyzes Memory Access Protocol (MAP)
│   │
│   └── metric_analysis.ml
│       └── Generic metric analysis framework
│           └── Used by: bank_conflicts, uncoalesced_access metrics
│
├── [CODE GENERATION]
│   ├── ra_compiler.ml
│   │   └── Compiles GPU analysis → Resource Calculus program
│   │
│   └── linearize_index.ml
│       └── Linearizes multi-dimensional array indices
│
├── [CONFIGURATION & UTILITIES]
│   ├── cost.ml
│   │   └── Cost value representation (exact/approximate)
│   │
│   ├── metric.ml
│   │   └── Metric specification and selection
│   │
│   ├── config.ml
│   │   └── Configuration parameters (memory word size, etc.)
│   │
│   └── uniform_range.ml
│       └── Range analysis for loop bounds

test/
└── test_metric_analysis.ml
```

---

## Key Concepts Implemented

### 1. **Memory Access Protocols (MAP)**
- **Concept**: Abstract representation of GPU memory access patterns
- **Code location**: `protocol_analysis.ml`
- **Implementation**: Captures thread-divergent control flow and per-thread memory accesses

### 2. **Warp-Level Semantics**
- **Concept**: GPU execution where 32 threads execute in lockstep, with some disabled by divergence
- **Code location**: `warp.ml`, `transaction.ml`
- **Implementation**:
  ```
  Thread states: enabled[] → which threads participate
  Transactions: groups of accesses to same memory bank
  Max transaction count → bank conflict cost
  ```

### 3. **Vectorized Expressions**
- **Concept**: Compact representation of per-iteration costs
- **Code location**: `vectorized.ml`
- **Implementation**: Expressions that parameterize over vector indices and enabled thread sets

### 4. **Metric Analysis**
- **Concept**: Framework to extract specific resource usage (bank conflicts, uncoalesced accesses)
- **Code location**: `metric_analysis.ml`
- **Parameters**:
  - `metric`: The specific resource to analyze (bank conflicts, uncoalesced accesses)
  - `relation`: Predicate relating GPU and sequential program costs

### 5. **Relational-Cost Analysis**
- **Concept**: Formal connection between GPU kernel and sequential program costs
- **Code location**: `ra_compiler.ml` (GPU → RA translation)
- **Result**: Sequential program whose cost analysis gives GPU kernel cost bounds

### 6. **Cost Representation**
- **Concept**: Exact vs. approximate costs
- **Code location**: `cost.ml`
- **Structure**: `{ value: int; exact: bool; state: Transaction.t option }`
- **Meaning**:
  - `exact = true`: Cost is provably exact (no approximations)
  - `exact = false`: Cost involves approximation (upper bound)
  - `state`: Links cost back to GPU execution state

---

## Analysis Flow: Detailed Steps

### Step 1: Protocol Analysis
```
Input: MAP
Output: Vectorized expression for per-iteration cost

Process:
1. For each loop iteration:
   a. Determine which threads are enabled (divergence analysis)
   b. Extract memory access indices for each thread
   c. Analyze memory access patterns
   d. Compute per-iteration cost contribution
2. Build vectorized expression combining all iterations
```

### Step 2: Metric Analysis
```
Input: Vectorized expression + metric type
Output: Metric-specific cost

Process (metric-dependent):
- Extract relevant access information (bank indices, addresses, enabled flags)
- Analyze access patterns to identify conflicts or inefficiencies:
  - Bank conflicts: Group accesses by bank ID, count maximum transaction count per bank
  - Uncoalesced accesses: Check if addresses form coalesced pattern, count separate transactions
- Return excess transaction count as cost
```

### Step 3: Resource Calculus Compilation
```
Input: GPU analysis results
Output: Annotated sequential RA program

Process:
1. Create sequential program mirroring GPU kernel structure
2. Annotate each statement with its cost (symbolic expression)
3. Mark exactness of each annotation
4. Collect statistics: exact vs. approximate components

Result:
for i = 0 to n {  // loop cost: 5*n
  x = x + 1;     // index cost: 1 (exact)
  if (x < m) {   // condition cost: m (approximate)
    y = y + x;   // memory cost: 2*i (exact)
  }
}
Total cost: 8*n + m
```

### Step 4: Symbolic Cost Solving
```
Input: Annotated RA program
Output: Final symbolic cost formula

Process:
1. Send RA program to external solver (Maxima, ABSynth, etc.)
2. Solver performs loop analysis:
   - Find closed-form cost formulas
   - Solve recurrence relations
   - Simplify expressions
3. Return result: final cost bound
```

---

## Exactness Tracking

The analysis maintains exactness information throughout:

```
Cost level:
├─ Exact: proven equal to GPU kernel cost
├─ Approximate: proven upper bound
└─ Unknown: incomplete analysis

Component level:
├─ Index analyses: cost of array access computation
├─ Loop analyses: cost of loop iteration
└─ Condition analyses: cost of conditional branches

Breakdown:
- exact_count: components with proven exact costs
- approximate_count: components overestimated
- Decision: only declare "exact" if all components are exact
```

---

## External Solver Integration

The analysis pipeline delegates symbolic cost computation to established tools:

```
RA Program + Annotations
    │
    ├─→ Maxima: Computer algebra system
    │   └─ Symbolic simplification
    │
    ├─→ ABSynth: Synthesis of cost bounds
    │   └─ Invariant generation
    │
    ├─→ CoFloCo: Complexity analysis
    │   └─ Loop complexity
    │
    └─→ KOAT: Termination and cost analysis
        └─ Symbolic resource analysis
```

Each tool can be enabled/disabled via command-line flags (use_maxima, use_absynth, etc.).

---

## File Organization

```
rel_cost/
├── README.md                 # High-level overview (this file)
├── ARCHITECTURE.md          # Detailed architecture (this file)
│
├── bin/                      # Executables
│   ├── cost.ml             # Pico: static relational-cost analysis
│   └── cost_dyn.ml         # Dynamic analysis tool
│
├── lib/                      # Core library
│   ├── [Semantics]
│   │   ├── warp.ml         # Warp execution
│   │   ├── vectorized.ml   # Vectorized expressions
│   │   └── transaction.ml  # Memory transactions
│   │
│   ├── [Analysis]
│   │   ├── protocol_analysis.ml # MAP analysis
│   │   └── metric_analysis.ml   # Metric extraction
│   │
│   ├── [Code Generation]
│   │   ├── ra_compiler.ml    # GPU → RA compilation
│   │   └── linearize_index.ml # Index linearization
│   │
│   └── [Configuration]
│       ├── cost.ml
│       ├── metric.ml
│       ├── config.ml
│       └── uniform_range.ml
│
└── test/                     # Unit tests
    └── test_metric_analysis.ml
```

---

## Connection to Paper Sections

| Paper Section | Concept | Code Location |
|---|---|---|
| Overview | GPU execution model, warps, thread divergence | `warp.ml`, `vectorized.ml` |
| Dynamic Cost Model | Warp semantics, transaction model | `transaction.ml`, `warp.ml` |
| Dynamic Cost Model | Per-iteration cost representation | `vectorized.ml` |
| Static Relational-Cost Analysis | GPU → RA translation | `ra_compiler.ml` |
| Static Relational-Cost Analysis | Metric analysis framework | `metric_analysis.ml` |
| Meta-Theory And Soundness | Exactness tracking | `cost.ml` (exact field) |
| Pico: Cost Analysis For GPU Kernels | Tool implementation | `bin/cost.ml`, solvers integration |
