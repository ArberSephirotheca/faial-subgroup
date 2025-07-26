# Symbolic Metric Analysis Implementation Work Log

## Summary

This work evolved the unique access analysis from an O(n²) pairwise constraint approach to an SMT-based system using Z3's distinct primitive. The implementation revealed performance bottlenecks with SMT solving at scale, leading to the development of alternative constraint formulations.

### Key Evolution
1. **Initial state**: O(n²) manual pairwise inequality constraints, timing out on larger thread counts
2. **SMT integration**: Added `Distinct` constructor to leverage Z3's built-in primitive - improved correctness but performance remained intractable for 16+ threads
3. **Ordered constraints**: Implemented chain-based approach (tid0 < tid1 < tid2 < ...) making analysis tractable up to 32 threads
4. **Optimization**: Added warp-uniform variable detection to reduce constraint complexity and bank count normalization for proper uncoalesced access semantics

### Performance Insights
The bottleneck was the interaction between `Distinct` constraints and thread-local conditions, not the constraint system itself. Removing thread uniqueness constraints allowed 32-thread analysis in 0.13 seconds, while the full constraint system with ordered chains completed in 16 seconds versus timeout with `Distinct`.

## Phase 1: Adding Distinct Constructor to Expression Language

### Objective
Add `Distinct of nexp list` constructor to `bexp` datatype to use Z3's built-in distinct primitive instead of manual pairwise constraints.

### Implementation
Added `Distinct of nexp list` to the expression language with Z3 integration:
```ocaml
| Distinct exprs ->
    let z3_exprs = List.map (n_to_expr ctx) exprs in
    Boolean.mk_distinct ctx z3_exprs
```

Updated pattern matching across 10 modules in the codebase to handle the new constructor. Renamed existing `distinct` function to `thread_distinct` to avoid naming conflict.

## Phase 2: Performance Analysis

### Performance Measurements
Using condition `tidx % 2 == 0` with maximize strategy and `Distinct` approach:
- 2 threads: 0.17 seconds
- 4 threads: 0.20 seconds  
- 8 threads: 3.01 seconds
- 16 threads: timeout after 600 seconds
- 32 threads with uniqueness constraint removed: 0.13 seconds

### Key Finding
The `Distinct` constraint with thread-local conditions creates complex SMT formulas. The rest of the constraint system scales to 32 threads in 0.13 seconds, indicating the bottleneck is specifically the interaction between `Distinct` and thread-local conditions.

## Phase 3: Alternative Constraint Formulation

### Objective
Develop tractable alternative to `Distinct` constraints for thread uniqueness.

### Implementation of Ordered Chain Constraints
Created `unique_tid_constraint_2` using ordered chain approach:
```ocaml
let unique_tid_constraint_2 (cfg:Config.t) : bexp =
  let thread_ids = List.init cfg.threads_per_warp (fun i -> thread_id (string_of_int i) cfg) in
  let rec make_chain = function
    | [] | [_] -> b_true
    | tid1 :: tid2 :: rest -> b_and (n_lt tid1 tid2) (make_chain (tid2 :: rest))
  in
  make_chain thread_ids
```

### Performance Results
Using same test condition with ordered constraints:
- 4 threads: 0.17 seconds
- 8 threads: 0.20 seconds  
- 16 threads: 0.69 seconds
- 32 threads: 16 seconds

Ordered chain approach completes 32-thread case in 16 seconds versus timeout (>600 seconds) with `Distinct`.

## Phase 4: Key Optimizations

### Warp-Uniform Variable Optimization
Refactored constraint generation to only create variables for warp-divergent thread IDs using `Config.is_warp_uniform` checks:
```ocaml
let tid_x =
  if Config.is_warp_uniform Variable.tid_x cfg then
    Num 0
  else
    tid_x
```
This reduces constraint complexity when certain thread dimensions are uniform across the warp.

### Bank Count Normalization
Added bank count normalization in the `ua` function to match uncoalesced access definition:
```ocaml
let formula = encode_ua cfg locals cond (n_div index (Num cfg.bank_count)) in
```
This uses M (denominator) as the number of array elements accessed by a global read, per the definition of uncoalesced accesses.

### Thread-Local Variable Handling
Updated `ua` function to properly handle thread-local variables:
```ocaml
let locals =
  Variable.Set.union
    (Variable.Set.diff locals Variable.tid_set)
    (thread_locals_set cfg)
```

## Phase 5: Adding UncoalescedAccesses2 Metric

### Objective
Provide SMT-based uncoalesced access analysis as alternative to heuristic-based approach.

### Implementation
Added `UncoalescedAccesses2` constructor to `Metric.t` with CLI string `"ua2"`. Connected to `run_ua2` function which uses `Symbolic_metric_analysis.ua` for SMT-based analysis instead of heuristics.

### Testing
CLI accepts `--metric=ua2` option. Test: `./faial-cost --metric=ua2 --only-cost examples/bc/ua-aligned-1.cu` returns `4`.

## Technical Outcomes

### Constraint System Architecture
- `unique_tid_constraint_1`: Uses `Distinct` primitive, accurate but slow with thread-local conditions
- `unique_tid_constraint_2`: Uses ordered chain constraints, faster scaling
- Rest of constraint system: Scales well to 32+ threads

### Metric Analysis Options
- `--metric=ua`: Heuristic-based uncoalesced access analysis
- `--metric=ua2`: SMT-based uncoalesced access analysis

## Phase 6: Config Field Correction

### Objective
Fix incorrect field usage in uncoalesced access analysis where `cfg.bank_count` was being used instead of proper memory segment calculation.

### Issue Resolution
The original implementation incorrectly used `cfg.bank_count` for memory segment normalization in SMT analysis. The correct approach uses `cfg.bytes_per_word` (default 4) to compute memory transaction granularity.

### Implementation
Added `Config.memory_segments_bits` function:
```ocaml
let memory_segments_bits (cfg:t) : int =
  8 * cfg.bytes_per_word (* 8 represents number of bits per byte *)
```

Updated both heuristic and SMT analyses to use consistent memory segment calculation:
- `index_analysis.ml`: Changed `8 * cfg.bytes_per_word` to `Config.memory_segments_bits cfg`
- `symbolic_metric_analysis.ml`: Changed `cfg.bank_count` to `Config.memory_segments_bits cfg`

This ensures both analysis approaches use the same memory transaction granularity definition based on the actual data type size rather than shared memory bank count.

## Phase 6.5: Theorem Proving DSL Development

### Objective
Develop a theorem-proving domain-specific language (DSL) to formally verify exact cost properties instead of just finding optimization bounds.

### Motivation
The existing `ua` function using `optimize_expr` can only find maximum/minimum bounds for uncoalesced access costs. However, for rigorous analysis, we need to prove exact cost properties like "expression X has exactly cost Y under condition Z."

### Implementation
Added three new modules to `symbolic_metric_analysis.ml`:

#### Comparison Module
```ocaml
module Comparison = struct
  type t = Equal | LessEqual | GreaterEqual | Less | Greater
  let to_relation : t -> (nexp -> nexp -> bexp) = function
    | Equal -> n_eq | LessEqual -> n_le | (* ... *)
end
```

#### ProofResult Module  
```ocaml
module ProofResult = struct
  type t = 
    | Proved 
    | Counterexample of Z3.Model.model 
    | Unknown of string
end
```

#### Theorem Module
```ocaml
module Theorem = struct
  type t = {
    cfg: Config.t; locals: Variable.Set.t;
    thread_context: bexp; global_context: bexp;
    index: nexp; comparison: Comparison.t; expected_cost: nexp;
  }
  
  let prove (thm : t) : ProofResult.t = (* Uses solve instead of optimize_expr *)
end
```

### Key Innovation: Proof by Negation
Instead of using `optimize_expr` to find bounds, `Theorem.prove` uses `solve` to check if the negation of the desired property is unsatisfiable:
- **Goal**: Prove `cost(index) = expected_cost`
- **Method**: Show that `NOT(cost(index) = expected_cost)` is UNSAT
- **Result**: If UNSAT, the theorem is proven; if SAT, we get a counterexample

### Testing
Added comprehensive test suite in `test_symbolic_metric_analysis.ml` with theorem proving capabilities:
```ocaml
prove_theorem "2 * threadIdx.x should have exact cost 2" {
  index = n_mult (Num 2) (Var Variable.tid_x);
  comparison = Comparison.Equal;
  expected_cost = Num 2;
  (* ... *)
}
```

### Performance Limitations Discovered
Initial testing revealed that the theorem prover does not scale to interesting, complex theorems due to the underlying constraint system complexity. Simple theorems like "2 * threadIdx.x = 2" work, but more sophisticated properties involving symbolic variables and complex conditions face the same performance bottlenecks as the underlying `ua` analysis.

### Impact on Project Direction
This performance limitation motivated **Phase 7: Linear Thread ID Architecture**. The theorem proving DSL demonstrates the need for a fundamentally faster constraint system to enable formal verification of realistic cost properties.

## Phase 7: Linear Thread ID Architecture (Planned)

### Motivation
The theorem proving DSL revealed that current constraint system performance prevents verification of interesting cost properties. The overarching goal is: **How can we make metric analysis fast enough to enable practical theorem proving?**

### Objective
Fundamentally redesign the thread modeling approach to use linear thread IDs instead of coordinate-based representations, eliminating the need for complex uniqueness and same-warp constraints.

### Core Design Principles
The current approach leaves thread coordinates with many degrees of freedom, requiring complex constraints to enforce uniqueness and warp membership. SAT solvers perform better with explicit, deterministic formulations.

### Proposed Architecture

#### Thread Identification Model
- **uid (thread within warp)**: Ranges from `0` to `threads_per_warp - 1`, used as variable suffix
- **warp_id**: Symbolic variable ranging from `0` to `total_warps - 1` 
- **thread_id**: Computed as `warp_id * threads_per_warp + uid`

This ensures global thread uniqueness by construction and makes warp membership explicit.

#### Coordinate Conversion
Convert from linear `thread_id` back to CUDA coordinates:
```ocaml
let threadIdx_x = thread_id mod block_dim.x in
let threadIdx_y = (thread_id / block_dim.x) mod block_dim.y in  
let threadIdx_z = thread_id / (block_dim.x * block_dim.y) in
```

#### Implementation Strategy
1. **Common Context Structure**: Create shared struct for `(suffix, uid, cfg)` parameters to enable clean function signatures and refactoring opportunities
2. **Function Replacement**: Replace current `thread_id(suffix, cfg)` with `thread_id(uid, warp_id, cfg)`
3. **Constraint Elimination**: Remove `unique_tid_constraint_*` and `same_warp_constraint` functions entirely
4. **Bounds Simplification**: Update `warp_constraints` to only include necessary bounds checking

#### Unsupported Cases
Block dimensions that don't align with warp boundaries (e.g., `block_dim.x = 17` with `threads_per_warp = 32`) will be flagged as unsupported with appropriate warnings. This is consistent with typical CUDA programming practices.

#### Dual Encoding Strategy
Maintain both the current constraint-based encoding and the new linear ID encoding for:
- Performance comparison and validation
- Future refactoring opportunities
- Backward compatibility during transition

### Expected Benefits
- **Constraint Reduction**: Eliminates O(n²) uniqueness constraints and same-warp logic
- **Performance Improvement**: More explicit thread model should improve SAT solver efficiency
- **Correctness by Construction**: Thread uniqueness and warp membership emerge naturally from the linear ID formulation
- **Cleaner Architecture**: Simpler constraint system with fewer degrees of freedom

### Future Refactoring Considerations
The dual encoding approach will enable systematic performance analysis and provide a foundation for future architectural improvements to the constraint generation system.

## Open Questions

- **Z3 timeout behavior**: Z3 uses 600-second default timeout when no explicit timeout specified, causing 10-minute delays in development