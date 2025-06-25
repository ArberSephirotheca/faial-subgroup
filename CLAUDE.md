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