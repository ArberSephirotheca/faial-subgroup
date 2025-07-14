# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Faial is a static analysis tool for finding bugs in CUDA kernels, particularly focused on data-race freedom (DRF) analysis and bank conflict detection. The project is written in OCaml and uses Dune as the build system.

## Common Development Commands

### Build Commands
```bash
make              # Build all binaries
make build        # Build with dune
make clean        # Clean build artifacts
./configure.sh    # Install dependencies (run once)
```

### Test Commands
```bash
make test         # Run unit tests with dune
make sys-test     # Run system tests
make build-test   # Build test suite
```

### Individual Binary Targets
```bash
make faial-drf         # Data-race freedom analysis tool
make faial-bc          # Bank conflict analysis tool
make faial-sync        # Barrier divergence analysis tool
make faial-cost        # Cost analysis tool
make faial-gen         # Code generation tool
make c-ast             # C AST parser
make wgsl-ast          # WGSL AST parser
```

## Architecture Overview

### Core Library Structure
- **stage0/**: Common utilities and foundational modules (logging, file I/O, JSON handling, subprocess management)
- **protocols/**: Core protocol definitions and memory access patterns
- **inference/**: Takes C AST and generates Memory Access Protocols
- **imp/**: Intermediate representation and transformations

### Analysis Modules
- **drf/**: Data-race freedom analysis pipeline with 7-step transformation:
  1. wellformed.ml: Convert to well-formed terms
  2. phasealign.ml: Align protocols  
  3. phasesplit.ml: Phase splitting
  4. locsplit.ml: Location splitting
  5. flatacc.ml: Flatten control flow
  6. symbexp.ml: Generate boolean expressions
  7. gensmtlib2.ml: Generate SMT queries

- **bank_conflicts/**: Bank conflict analysis for GPU shared memory
- **barrier_div/**: Barrier divergence analysis
- **total_cost/**: Cost analysis and optimization

### Language Support
- **C/CUDA**: Primary target language via c-to-json parser
- **WGSL**: WebGPU Shading Language support

## Dependencies

OCaml dependencies (installed via `./configure.sh`):
- dune 3.16.0 (build system)
- z3 4.13.0 (SMT solver)
- yojson 2.2.2 (JSON handling)
- ANSITerminal 0.8.5 (terminal colors)
- cmdliner 1.3.0 (CLI interface)

External dependencies:
- c-to-json (C parser, must be installed separately)
- LLVM/Clang development libraries

## Testing

Examples are organized by analysis type:
- `examples/bc/`: Bank conflict test cases
- `examples/drf/`: Data-race freedom test cases  
- `examples/wgsl/`: WGSL test cases
- `examples/approx/`: Approximation analysis test cases

Test a specific kernel:
```bash
./faial-drf examples/drf/drf-saxpy.cu
./faial-bc examples/bc/2tid.cu
```

## Development Workflow

1. Run `./configure.sh` once to install dependencies
2. Use `make` to build all binaries
3. Test changes with `make test` 
4. For specific analysis development, focus on the relevant module (drf/, bank_conflicts/, etc.)
5. Add test cases to appropriate examples/ subdirectory

## Git Commit Guidelines

When creating commits, do NOT include:
- Co-Authored-By tags
- "Generated with Claude Code" footers
- Any AI attribution in commit messages

Use clear, descriptive commit messages that focus on the actual changes made.

## Documentation Writing Guidelines

When writing conceptual documentation for this project, follow these principles to create clear, useful explanations:

### Intent-First Communication
- Lead with the "why" before the "how" - establish what we're trying to accomplish before explaining methods
- Help readers understand the reasoning behind design decisions and transformations
- Structure explanations as: purpose/intent → specific techniques used to achieve that purpose

### Concrete Specificity Over Abstract Generality
- Replace vague programming terminology with domain-specific explanations of actual transformations and analyses
- Use concrete examples when possible (e.g., "`array[i][j]` becomes `array[i*width + j]`")
- Avoid generic terms like "functionality that flows" or "modules provide capabilities"
- Use descriptive section titles that summarize content essence rather than generic labels

### Established Domain Language
- Use the project's established terminology consistently (e.g., "Memory Access Protocol" not "CUDA kernel represented as a protocol")
- Leverage known terms that don't need re-explanation for clearer communication
- Prefer domain-specific language over programming terms (e.g., "memory access patterns," "precision tracking" over "module interactions," "data flows")

### Conceptual Focus Over Implementation Details
- Explain **what the system does and how it works** rather than how to use APIs or navigate code
- Describe the intellectual approach and problem-solving strategy rather than code organization
- Focus on understanding the analytical process: "how does the system think about the problem"
- Help readers mentally model how the system works and why it makes specific decisions

### Clarity Through Structure
- Organize information to build understanding progressively
- Every section should teach concepts that help readers reason about what the system does and why it works
- Avoid organizational sections that just categorize information without explaining substance
- Rewrite confusing sentences clearly, even if it requires more words
- Eliminate unclear metaphors and vague descriptions

### Eliminate Redundant Qualifiers
- Remove unnecessary phrases that don't add meaning (e.g., "throughout the pipeline" when pipeline IS the analysis)
- Question every qualifier - does it actually clarify or just add verbosity?
- Choose the most direct expression: "analysis decisions" → "analysis"

### Use Domain-Precise Terminology
- Replace generic technical terms with domain-specific ones that convey exact meaning
- "Precision tracking" → "overapproximation tracking" (explains what kind of precision we care about)
- "Mathematical results" → "exact costs" (specifies what mathematical concept matters)
- Connect to established standards and well-known concepts from the problem domain (e.g., C's `sizeof` operator)

### Question Every Abstract Term
- Challenge vague terms like "configuration parameters" - be specific about what parameters and why they matter
- Replace generic programming concepts with concrete domain actions
- Ask: "What specifically does this accomplish?" rather than "How is this implemented?"
- Focus on analytical reasoning rather than system mechanics - implementation details like error handling are often too low-level for architectural documentation

### Avoid Vague Pronouns
- Avoid using "it" when a clear subject noun works better
- Example: "converts it into a form" → "converts the protocol into a form"
- This improves clarity and reduces ambiguity about what is being referenced

### Structure with Clear Roadmaps
- Lead sections with roadmaps that list what will be covered
- Use format: "To achieve [goal], several [actions] are needed: [A], [B], [C]"
- Example: "To prepare Memory Access Protocols for Resource Calculus Generation, several transformations are needed: array filtering, constant folding and propagation, memory access flattening, and loop uniformization"
- This gives readers immediate orientation before diving into details

### Connect Stages Through Requirements
- Explain each stage in terms of what the next stage requires
- Show how current stage constraints are driven by subsequent stage needs
- Avoid subjective qualifiers like "most sophisticated" - use objective descriptions
- Focus on necessity and causation rather than implementation complexity

### Use Parenthetical Clarification
- Clarify technical terms with parenthetical explanations
- Example: "hardware-level memory accesses (multi-dimensional arrays converted to flat memory addresses)"
- This helps readers understand domain-specific terminology without lengthy explanations

### Advanced Documentation Patterns

#### Intent-First Problem-Solution Structure
- When explaining complex transformations, follow the pattern: state objective → identify problem → present solution
- Don't mechanically describe what the code does - explain WHY the transformation is necessary
- Example: "To translate loops into summations [objective], the encoding must handle the mismatch between loop ranges (which can have arbitrary step values) and mathematical summations (which iterate over every element) [problem]. The solution applies the inverse of the step function [solution]."

#### Define Before Use
- Always define technical terms before using them in explanations
- Introduce concepts when they first become relevant, not when they're first used
- Example: Define "step value (the amount added or multiplied per iteration)" before referring to "arbitrary step values"

#### Precise Terminology Consistency
- Use "translate" for converting between representations in translation contexts
- Use "encode" for encoding processes  
- Avoid generic terms like "convert" that may have other technical meanings
- Replace vague subjects like "the system" with specific actors: "the translation", "the encoding", or "we"
- Be consistent with technical vocabulary throughout the document

#### Mathematical Formalization with Examples
- When presenting mathematical concepts, provide both formal definitions and concrete examples
- Make abstract concepts tangible through specific instances
- Example: Provide both the formal bank conflict formalization and concrete examples with specific index values

#### Eliminate Unnecessary Qualifiers
- Avoid adjectives like "complex", "sophisticated", "mathematical" unless they add specific technical meaning
- Be concrete rather than descriptive - focus on what something does, not subjective characterizations
- Question every adjective: does it clarify the technical concept or just add noise?

#### Scope Limitations Up Front
- State what's outside the document's scope early to set proper expectations
- Example: "Discussion of CoFloCo and KoAT is outside the scope of this document"

#### Case-by-Case Structure for Algorithms
- For algorithms with multiple cases, use numbered cases with concrete examples
- Structure as: "Case 1: [condition] such as [example]. [What happens and why]."
- This is clearer than abstract algorithmic descriptions

#### Connect to Established Techniques
- When using standard algorithms or transformations from established fields, explicitly mention the connection
- Reference the standard name/terminology from the relevant domain (compilers, databases, algorithms, etc.)
- This helps readers understand that the technique is well-established rather than novel
- Example: "We apply loop index normalization, a standard compiler technique, to convert loops with arbitrary step sizes into unit-stride loops"
- This grounds the work in existing knowledge and shows appropriate use of established methods

The overarching goal is **conceptual clarity**: writing that helps someone understand the system's analytical reasoning and approach to solving problems, enabling them to mentally model how the system works rather than just describing its structure or features.