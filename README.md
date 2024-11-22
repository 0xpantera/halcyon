# Halcyon: A C Compiler in Haskell

Halcyon is a work-in-progress compiler for a large subset of C, written in Haskell. It targets the x86_64 instruction set architecture. This project focuses on implementing the core compiler functionality while leveraging existing system tools for preprocessing, assembly, and linking.

## Current Status

The compiler currently handles C programs with unary operators and integer constants. For example:

```c
int main(void) {
    return ~(-42);
}
```

### Compilation Pipeline

The compiler processes source code through the following stages:

1. **Lexical Analysis**: Breaks source code into a sequence of tokens
2. **Parsing**: Converts tokens into an Abstract Syntax Tree (AST)
3. **TACKY Generation**: Transforms AST into TACKY intermediate representation
4. **Code Generation**: Transforms AST into x86_64 assembly
5. **Code Emission**: Outputs the assembly code to an executable

### Internal Representations

Programs are represented internally using a series of increasingly lower-level data structures:

1. **Abstract Syntax Tree (AST)**:
   ```haskell
   data Program = Program FunctionDef
   data FunctionDef = Function 
     { name :: Text
     , body :: Statement 
     }
   data Statement = Return Expr
   data Expr = Constant Int | Unary UnaryOp Expr
   data UnaryOp = Complement | Negate
   ```

2. **TACKY IR**:
  ```haskell
  data Program = Program FunctionDef
  data FunctionDef = Function 
    { name :: Text
    , body :: [Instruction]
    }
  data Instruction 
    = Return TackyVal
    | Unary UnaryOp TackyVal TackyVal
  data TackyVal = Constant Int | Var Text
  ```

3. **Assembly AST**:
   ```haskell
   data Program = Program FunctionDef
   data FunctionDef = Function 
     { name :: Text
     , instructions :: [Instruction]
     }
   data Instruction = Mov Operand Operand | Ret
   data Operand = Imm Int | Register
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
│       ├── Backend/               # Code generation and emission
│       │   ├── Codegen.hs         # TACKY to Assembly conversion
│       │   ├── Emit.hs            # Assembly to text output
│       │   └── ReplacePseudos.hs  # Register/stack allocation
│       ├── Core/                  # Core data types and utilities
│       │   ├── Assembly.hs        # Assembly representation
│       │   ├── Ast.hs             # C language AST
│       │   ├── Monad.hs           # Compiler monad stack
│       │   ├── Settings.hs        # Compiler settings and types
│       │   ├── Tacky.hs           # TACKY IR definition
│       │   └── TackyGen.hs        # AST to TACKY transformation
│       ├── Driver/                # Compiler driver
│       │   ├── Cli.hs             # Command line interface
│       │   └── Pipeline.hs        # Compilation pipeline
│       └── Frontend/              # Parsing and analysis
│           ├── Lexer.hs           # Lexical analysis
│           ├── Parse.hs           # Parsing
│           └── Tokens.hs          # Token definitions
├── test/                          # Test suite
│   └── Main.hs
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
- [ ] Unary operators
- [ ] Binary operators
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