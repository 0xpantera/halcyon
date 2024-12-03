# Halcyon: A C Compiler in Haskell

Halcyon is a work-in-progress compiler for a large subset of C, written in Haskell. It targets the x86_64 instruction set architecture. This project focuses on implementing the core compiler functionality while leveraging existing system tools for preprocessing, assembly, and linking.

## Current Status

The compiler currently handles C programs with unary operators and integer constants. For example:

```c
int main(void) {
    return -((42 * 10) / (2 + 3));  // Returns -84
}
```

### Compilation Pipeline

The compiler is organized into several major subsystems:

- **Frontend** - Parsing and analysis
  - Lexical analysis (breaks source into tokens)
  - Parsing (converts tokens to AST)
  - Token definitions

- **Core** - Core data types and compiler infrastructure
  - AST, Assembly, and TACKY intermediate representations
  - Compiler monad and error handling

- **Backend** - Code generation
  - TACKY to assembly conversion
  - Register allocation
  - Assembly output

- **Driver** - Pipeline coordination
  - Command line interface
  - Compilation pipeline stages
  - External tool integration (GCC for preprocessing and linking)

Each subsystem is organized as a hierarchical module that provides a clean interface to its functionality while hiding implementation details.

### Internal Representations

Programs are represented internally using a series of increasingly lower-level data structures:

1. **Abstract Syntax Tree (AST)**:
  ```haskell
  data Program = Program Function
  data Function = Function Text Statement
  data Statement = Return Expr
  data Expr
    = Constant Int
    | Unary UnaryOp Expr
    | Binary BinaryOp Expr Expr
  data UnaryOp = Complement | Negate
  data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
  ```

2. **TACKY IR**:
  ```haskell
  data Program = Program Function
  data Function = Function Text [Instruction]
  data Instruction
    = Return Val
    | Unary UnaryOp Val Val
    | Binary BinaryOp Val Val Val
  data Val = Constant Int | Var Text
  data UnaryOp = Complement | Negate
  data BinaryOp = Add | Subtract | Multiply | Divide | Remainder
  ```

3. **Assembly AST**:
  ```haskell
  data Program = Program Function
  data Function = Function Text [Instruction]
  data Instruction
    = Mov Operand Operand
    | Unary UnaryOp Operand
    | Binary BinaryOp Operand Operand
    | Idiv Operand
    | Cdq
    | AllocateStack Int
    | Ret
  data Operand 
    = Imm Int 
    | Register Reg
    | Pseudo Text
    | Stack Int
  data UnaryOp = Neg | Not
  data BinaryOp = Add | Sub | Mult
  data Reg = Ax | DX | R10 | R11
  ```


## Project Structure

  ```
  .
  ├── app/                           # Application entry point
  │   └── Main.hs
  ├── bin/                           # Binary outputs
  ├── lib/                           # Main library code
  │   ├── Halcyon.hs                 # Library entry point
  │   └── Halcyon/                   # Core modules
  │       ├── Backend.hs             # Backend subsystem interface
  │       ├── Backend/               # Code generation and emission
  │       │   ├── Codegen.hs         # TACKY to Assembly conversion
  │       │   ├── Emit.hs            # Assembly to text output
  │       │   └── ReplacePseudos.hs  # Register/stack allocation
  │       ├── Core.hs                # Core subsystem interface
  │       ├── Core/                  # Core data types and utilities
  │       │   ├── Assembly.hs        # Assembly representation
  │       │   ├── Ast.hs             # C language AST
  │       │   ├── Monad.hs           # Compiler monad stack
  │       │   ├── Settings.hs        # Compiler settings and types
  │       │   ├── Tacky.hs           # TACKY IR definition
  │       │   └── TackyGen.hs        # AST to TACKY transformation
  │       ├── Driver.hs              # Driver subsystem interface
  │       ├── Driver/                # Compiler driver
  │       │   ├── Cli.hs             # Command line interface
  │       │   └── Pipeline.hs        # Compilation pipeline
  │       ├── Frontend.hs          # Frontend subsystem interface
  │       └── Frontend/              # Parsing and analysis
  │           ├── Lexer.hs           # Lexical analysis
  │           ├── Parse.hs           # Parsing
  │           └── Tokens.hs          # Token definitions
  ├── test/                          # Test suite
  │   ├── Main.hs
  │   └── Test/
  │       ├── Lexer.hs
  │       ├── Parser.hs
  │       ├── Tacky.hs
  │       ├── Assembly.hs
  │       ├── Pipeline.hs
  │       └── Common.hs
  ├── CHANGELOG.md                   # Version history
  ├── LICENSE                        # Project license
  ├── README.md                      # Project documentation
  ├── flake.nix                      # Nix build configuration
  └── halcyon.cabal                  # Cabal build configuration
  ```

### Architecture

The compiler uses a monad transformer stack to handle IO operations and error management:

```haskell
newtype CompilerT m a = CompilerT 
  { unCompilerT :: ExceptT CompilerError m a }

type Compiler = CompilerT IO
```

This provides:
- Error handling through `ExceptT`
- IO capabilities through the underlying monad
- Clean separation of pure and effectful code
- Structured error reporting and recovery

## Command Line Interface

```bash
halcyon [OPTIONS] FILE

Options:
  --lex                 Run lexical analysis only
  --parse               Run parsing only
  --codegen             Run through code generation
  --tacky               Run through TACKY generation
  -S                    Stop after assembly generation
  -h,--help             Show help text
```

### Build and Run

```bash
# Build the project
cabal build

# Run the compiler
cabal run halcyon -- [OPTIONS] input.c

# Example: Compile a file
cabal run halcyon -- input.c

# Example: Run only the lexer
cabal run halcyon -- --lex input.c
```

## Testing

Halcyon uses Hspec and Tasty for its test suite. The tests cover all stages of compilation:

```bash
# Run all tests
cabal test

# Run tests with output
cabal test --test-show-details=direct

# Run a specific test module
cabal test --test-pattern "Lexer"
```

The test suite includes:

- Unit tests for each compiler stage
- Integration tests for the full pipeline
- Helper utilities for building test cases

Tests are organized by compiler stage in `test/Test/`:

- `Lexer.hs`: Token generation
- `Parser.hs`: AST construction
- `Tacky.hs`: TACKY IR generation
- `Assembly.hs`: Assembly generation
- `Pipeline.hs`: Full compilation pipeline
- `Common.hs`: Shared test utilities


## External Dependencies

Halcyon relies on the following system tools:
- **GCC**: For preprocessing C source files (`gcc -E`)
- **Assembler**: For converting assembly to object files
- **Linker**: For producing final executables

Make sure these tools are installed and available in your system path.

## Error Handling

The compiler provides detailed error reporting for:
- Lexical errors (invalid characters, malformed numbers)
- Syntax errors (invalid program structure)
- Semantic errors (coming soon)
- System errors (file I/O, external tool failures)

## Future Plans

### The Basics
- [x] A minimal compiler
- [x] Unary operators
- [x] Binary operators
- [ ] Logical and relational operators
- [ ] Local variables
- [ ] if statements and conditional expressions
- [ ] Compound statements
- [ ] Loops
- [ ] Functions
- [ ] File scope variable declarations and storage-class specifiers

### Types Beyond Int
- [ ] Long integers
- [ ] Unsigned integers
- [ ] Floating-point numbers
- [ ] Pointers
- [ ] Arrays and pointer arithmetic
- [ ] Characters and strings
- [ ] Supporting dynamic memory
- [ ] Structures

### Optimizations
- [ ] Optimizing TACKY programs
- [ ] Register Allocations

## Contributing

This is a personal learning project following the book "Writing a C Compiler" by Nora Sandler. While it's not currently open for contributions, feel free to use it as a reference for your own compiler projects.