# Revision history for halcyon

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
# Changelog for Halcyon

## 0.1.0.0 -- 2024-11-13

### Added
- Initial version of the compiler
- Support for basic C programs with return statements 
- Lexer using Megaparsec
- Recursive descent parser
- Defined CompilerT monad transformer
- x86_64 code generation
- Basic compiler driver with preprocessing and assembly

## 0.2.0.0 -- 2024-11-22

### Added
- Support for unary operators (negation and bitwise complement)
- New TACKY intermediate representation between AST and Assembly
- Stack frame management in generated assembly
- Function prologue and epilogue handling
- Pseudoregister allocation and replacement
- Invalid instruction detection and fixing
- New compiler passes in pipeline:
  - TACKY generation
  - Pseudoregister replacement
  - Stack allocation
  - Instruction fixing

### Changed
- Compiler pipeline now includes TACKY transformation stage
- Assembly generation now works from TACKY rather than directly from AST
- Parser improved to handle nested expressions correctly

## 0.2.1.0 -- 2024-11-25

### Added
- Comprehensive test suite with Hspec and Tasty
- Unit tests for all compiler stages:
  - Lexer tests
  - Parser tests
  - TACKY generation tests 
  - Assembly generation tests
  - Full pipeline integration tests
- Test utilities and helper functions in Test.Common

## 0.2.2.0 -- 2024-11-25

### Changed
- Improved type definitions across compiler stages
- Added record syntax for multi-field data constructors
- Standardized naming conventions in core types
- Simplified token type names
- Made qualified imports consistent across modules