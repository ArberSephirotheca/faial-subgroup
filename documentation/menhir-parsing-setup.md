# Menhir Parsing Project Setup Guide

This document describes how to set up Menhir parsing projects in the Faial codebase to avoid circular dependency issues.

## Problem

When creating Menhir parsers within an existing library, you may encounter circular dependency errors like:

```
Error: Module Tactics_parser in directory _build/default/protocols/lib
depends on Protocols.
This doesn't make sense to me.

Protocols is the main module of the library and is the only module exposed
outside of the library. Consequently, it should be the one depending on all
the other modules in the library.
```

This occurs because the parser tries to reference the main library module, creating a circular dependency.

## Solution: Separate Parsing Library

The solution is to create a separate parsing directory/library that depends on the main library but is separate from it.

## Step-by-Step Setup

### 1. Enable Menhir in dune-project

Ensure your `dune-project` file includes:

```dune
(lang dune 3.17)
(using menhir 2.0)
```

### 2. Create Parsing Directory Structure

Create a separate directory for parsing:

```
protocols/
├── lib/              # Main library
│   └── dune
├── parsing/          # New parsing library  
│   ├── dune
│   ├── tactics_lexer.mll
│   └── tactics_parser.mly
└── test/
```

### 3. Configure Parsing Library dune File

Create `protocols/parsing/dune`:

```dune
(menhir (modules tactics_parser))
(ocamllex tactics_lexer)
(library
 (name protocol_parsing)
 (libraries stage0 protocols))
```

Key points:
- `(menhir (modules tactics_parser))` - Generates parser from .mly file
- `(ocamllex tactics_lexer)` - Generates lexer from .mll file  
- Library name should be descriptive (e.g., `protocol_parsing`)
- Dependencies include the main library (`protocols`) - this is safe since parsing depends on main, not vice versa

### 4. Parser File Header (.mly)

In your `tactics_parser.mly` file header:

```ocaml
%{
open Protocols.Gen_z3
open Tactic
%}
```

You can safely open modules from the main library since the parsing library depends on it.

### 5. Lexer File Header (.mll)

In your `tactics_lexer.mll` file header:

```ocaml
{
open Tactics_parser
}
```

The lexer opens the generated parser module.

### 6. Using the Parser from Other Libraries

To use the parser from other libraries (like `rel_cost`), add the parsing library as a dependency:

```dune
(library
  (name rel_cost)
  (libraries
    ; 3rd-party
    z3 str
    ; 1st-party
    stage0 protocols protocol_parsing ra approx))
```

Then import it in your OCaml code:

```ocaml
open Protocol_parsing
(* Now you can use the parser functions *)
```

## Example: Tactics Parser Setup

Based on the tactics parser implementation:

### Directory Structure
```
protocols/
├── lib/
│   ├── dune                    # Main protocols library
│   └── gen_z3.ml              # Contains Tactic, Probe, Params modules
├── parsing/
│   ├── dune                    # Parsing library configuration
│   ├── tactics_lexer.mll       # Lexer for tactics language
│   └── tactics_parser.mly      # Parser for tactics language
```

### Key Files

**protocols/parsing/dune:**
```dune
(menhir (modules tactics_parser))
(ocamllex tactics_lexer)
(library
 (name protocol_parsing)
 (libraries stage0 protocols))
```

**tactics_parser.mly header:**
```ocaml
%{
open Protocols.Gen_z3
open Tactic
%}
```

**tactics_lexer.mll header:**
```ocaml
{
open Tactics_parser
}
```

## Benefits of This Approach

1. **Avoids circular dependencies** - Parsing library depends on main library, not vice versa
2. **Clean separation** - Parsing logic is isolated from core library functionality  
3. **Reusable** - Other libraries can depend on the parsing library without circular issues
4. **Maintainable** - Clear dependency hierarchy makes the codebase easier to understand

## Common Issues and Solutions

### Issue: "Unbound module" errors
**Solution:** Ensure the parsing library correctly depends on the main library and uses proper module paths (e.g., `Protocols.Gen_z3.Tactic`)

### Issue: Menhir/OCamllex not found
**Solution:** Add `(using menhir 2.0)` to your `dune-project` file

### Issue: Generated files not found
**Solution:** Ensure your dune file has both `(menhir (modules parser_name))` and `(ocamllex lexer_name)` rules

## Dependencies to Add

If not already present, add these to your `dune-project` dependencies:

```dune
(depends
  ; ... existing dependencies ...
  (menhir :build)
  ; ... rest of dependencies ...)
```

This approach successfully resolves circular dependency issues while maintaining a clean, modular architecture for parsing functionality.