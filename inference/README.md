# Inference

The *inference* stage (module `infer`) is a translation between a source language AST (eg, C/CUDA, WGSL) and the intermediate `Imp` representation used by Faial.

Our inference stage supports 2 ASTs: C/CUDA and WGSL. In both languages the AST is read from a JSON file, then inference takes the AST and generates an equivalent Imp, depicted as follows:

```
JSON → Source Language AST → Imp → Memory Access Protocol
```

For C/CUDA, this involves a two-stage transformation:
```
JSON (c-to-json) → C_lang → D_lang → Imp → Memory Access Protocol
```

For WGSL, this involves a single-stage transformation:
```
JSON (WGSL AST) → W_lang → Imp → Memory Access Protocol
```

**Module structure:**

- **`c_lang.ml`** - C/CUDA AST definitions and JSON parsing from c-to-json; mirrors LLVM's AST
- **`d_lang.ml`** - C/CUDA AST where memory-accesses are made into statements (not expressions); extracted from C_lang
- **`d_to_imp.ml`** - C/CUDA to Imp translation (via D_lang intermediate)
- **`w_lang.ml`** - WGSL AST definitions
- **`w_to_imp.ml`** - WGSL to Imp translation


## C/CUDA Pipeline: `c_lang → d_lang → d_to_imp → Imp`

**Modules**: `c_lang.ml`, `d_lang.ml`, `d_to_imp.ml`

**Key characteristics**:
- **Two-stage translation** required due to expression-embedded memory operations
- **JSON input** from c-to-json tool (LLVM AST dump)
- **Memory extraction** via state monad in `d_lang.ml`

The C/CUDA translation pipeline uses a unique two-stage approach that separates parsing concerns from analysis preparation:

#### Stage 1: C_lang (JSON Parser and Raw AST)
**Purpose**: Handle the complexity of parsing LLVM/Clang JSON output
- **Input**: JSON from c-to-json tool (LLVM AST dump)
- **Output**: Raw C/CUDA AST that faithfully mirrors LLVM's structure
- **Responsibilities**:
  - JSON parsing with comprehensive error handling
  - Direct mapping to LLVM AST node types (100+ expression variants)
  - Graceful handling of invalid/recovery expressions
  - Preservation of all source language constructs
  - Type system integration (`J_type.t` from JSON)

#### Stage 2: D_lang (Analysis-Ready Transformation)
**Purpose**: Extract memory operations from expressions into statements (required by Imp)
- **Input**: `C_lang.Program.t`
- **Output**: AST where all memory accesses are explicit statements
- **Core Challenge**: C allows memory operations within expressions, but Imp requires memory accesses to be statements
- **Transformations**:
  - **Memory Access Extraction**: Convert array subscripts and pointer dereferences from expressions to explicit read/write statements
  - **Expression Decomposition**: Split complex expressions containing memory operations into statement sequences
  - **Side Effect Isolation**: Use state monad to track and extract memory accesses during expression evaluation
  - **Statement Generation**: Generate explicit `ReadAccessStmt` and `WriteAccessStmt` for analysis

**Example transformation**:
```c
// C source: memory access within expression
int result = array[i] + array[j];

// C_lang: represents as nested expressions
BinaryOperator {
  opcode = "+";
  lhs = ArraySubscriptExpr {lhs=array; rhs=i};
  rhs = ArraySubscriptExpr {lhs=array; rhs=j}
}

// D_lang: extracts memory accesses as statements
ReadAccessStmt {target=temp1; source=array[i]; ty=int}
ReadAccessStmt {target=temp2; source=array[j]; ty=int}
AssignStmt {var=result; data=temp1 + temp2}
```

#### Why Two-Stage Architecture is Required

1. **What Imp Needs**:
   - All memory accesses must be explicit statements for analysis
   - Memory access patterns must be extractable for protocol generation
   - Temporal ordering of reads/writes must be preserved

2. **What C Provides**:
   - Memory operations embedded within expressions: `a[i] + b[j]`
   - Nested array subscripts: `arr[i][j][k]`
   - Pointer arithmetic mixed with computation: `*(ptr + offset) * 2`

3. **How We Bridge The Gap**:
   - **Two-stage translation**: C_lang → D_lang → Imp
   - **State monad transformation**: Extract memory operations from nested expressions
   - **Temporary variables**: Store results of extracted memory statements
   - **D_lang intermediate**: Isolates the complexity of memory operation extraction

## WGSL Pipeline: `w_lang → w_to_imp → Imp`

**Modules**: `w_lang.ml`, `w_to_imp.ml`

**Key characteristics**:
- **Single-stage translation** sufficient due to structured memory model
- **Direct AST input** (no complex JSON parsing needed)
- **Address space annotations** explicitly map to memory hierarchy
- **Structured types** enable straightforward field access flattening

