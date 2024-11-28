# Default recipe to show help
default:
    @just --list

# Build variables
binary := "halcyon"
test_dir := "writing-a-c-compiler-tests"

# Build the project
build:
    cabal build

# Build in watch mode - recompiles on file changes
watch:
    cabal build --enable-tests --file-watch

# Run compiler on a file with specific stage
run FILE STAGE="":
    cabal run {{binary}} -- {{FILE}} {{STAGE}}

# Run all tests
test:
    cabal test

# Run specific test file or directory
test-file FILE:
    cabal test --test-options="--match '{{FILE}}'"

# Run compiler test suite
test-compiler:
    cd {{test_dir}} && ./test_compiler ../bin/{{binary}}

# Run compiler test suite for specific chapter
test-chapter CHAPTER:
    cd {{test_dir}} && ./test_compiler ../bin/{{binary}} --chapter {{CHAPTER}}

# Run tests in watch mode
test-watch:
    cabal test --enable-tests --file-watch

# Check setup for compiler test suite
check-setup:
    cd {{test_dir}} && ./test_compiler --check-setup

# Format Haskell source files using ormolu
fmt:
    find lib test -name "*.hs" -exec ormolu --mode inplace {} \;

# Check formatting without making changes
fmt-check:
    find lib test -name "*.hs" -exec ormolu --mode check {} \;

# Run HLint suggestions
lint:
    hlint lib test

# Apply HLint suggestions automatically where possible
lint-apply:
    hlint lib test --refactor --refactor-options="--inplace"

# Update symlink to latest build (if needed)
link:
    rm -f bin/{{binary}}
    ln -s "$(cabal list-bin {{binary}})" bin/{{binary}}

# Clean build artifacts
clean:
    cabal clean
    rm -f bin/{{binary}}

# Clean and rebuild everything
rebuild: clean build

# Create new test file from template
new-test NAME:
    #!/usr/bin/env bash
    file="test/Test/{{NAME}}.hs"
    echo "{-# LANGUAGE OverloadedStrings #-}" > ${file}
    echo "module Test.{{NAME}} ({{NAME}}Specs) where" >> ${file}
    echo "" >> ${file}
    echo "import Test.Hspec" >> ${file}
    echo "" >> ${file}
    echo "{{NAME}}Specs :: Spec" >> ${file}
    echo "{{NAME}}Specs = describe \"{{NAME}}\" $ do" >> ${file}
    echo "  it \"placeholder test\" $ do" >> ${file}
    echo "    True \`shouldBe\` True" >> ${file}

# Generate documentation
docs:
    cabal haddock --enable-documentation

# Run ghci with project modules
repl:
    cabal repl

# Initialize a new test C file
new-c-test CHAPTER NAME:
    #!/usr/bin/env bash
    dir="{{test_dir}}/tests/chapter_{{CHAPTER}}/valid"
    file="${dir}/{{NAME}}.c"
    mkdir -p ${dir}
    echo "int main(void) {" > ${file}
    echo "    return 0;" >> ${file}
    echo "}" >> ${file}

# Show project dependency tree
deps:
    cabal-plan deps

# Update cabal index
update:
    cabal update

# Build project with profiling
build-profile:
    cabal build --enable-profiling

# Count lines of code
loc:
    find lib test -name "*.hs" | xargs wc -l

# Check for outdated dependencies
outdated:
    cabal outdated

# List all modules in the project
modules:
    find lib test -name "*.hs" -exec basename {} .hs \;

# Generate comprehensive project context for AI assistance
context *FILENAME="ai-context.txt":
    #!/bin/bash
    echo "<documents>" > {{FILENAME}}
    
    # Project Structure Document
    echo "<document index=\"1\">" >> {{FILENAME}}
    echo "<source>structure.txt</source>" >> {{FILENAME}}
    echo "<document_content>" >> {{FILENAME}}
    # Add git status and recent history
    echo "<git_status>" >> {{FILENAME}}
    git status -s >> {{FILENAME}}
    echo "</git_status>" >> {{FILENAME}}
    echo "<git_history>" >> {{FILENAME}}
    git log --oneline -n 10 >> {{FILENAME}}  # Last 10 commits
    echo "</git_history>" >> {{FILENAME}}
    # Add directory tree
    echo "<directory_tree>" >> {{FILENAME}}
    eza -T --git-ignore >> {{FILENAME}}
    echo "</directory_tree>" >> {{FILENAME}}
    echo "</document_content>" >> {{FILENAME}}
    echo "</document>" >> {{FILENAME}}

    # Cabal Configuration Document
    echo "<document index=\"3\">" >> {{FILENAME}}
    echo "<source>project-config.txt</source>" >> {{FILENAME}}
    echo "<document_content>" >> {{FILENAME}}
    cat halcyon.cabal >> {{FILENAME}}
    echo "</document_content>" >> {{FILENAME}}
    echo "</document>" >> {{FILENAME}}

    # Source Files Document
    echo "<document index=\"4\">" >> {{FILENAME}}
    echo "<source>source-files.txt</source>" >> {{FILENAME}}
    echo "<document_content>" >> {{FILENAME}}
    find . -name "*.hs" -type f \
        -not -path "./dist-newstyle/*" \
        -not -path "./writing-a-c-compiler-tests/*" \
        -not -path "./c-programs/*" \
        -exec sh -c 'echo "<file>{}" && echo "<content>" && cat "{}" && echo "</content>"' \; >> {{FILENAME}}
    echo "</document_content>" >> {{FILENAME}}
    echo "</document>" >> {{FILENAME}}

    echo "</documents>" >> {{FILENAME}}
    
    echo "Project context saved to {{FILENAME}}"

# Generate module dependency graph in DOT format
deps-graph *FILENAME="module-deps.dot":
    #!/bin/bash
    if ! command -v dot &> /dev/null; then \
        echo "graphviz not found. Install with: brew install graphviz"; \
        exit 1; \
    fi
    if ! command -v graphmod &> /dev/null; then \
        echo "graphmod not found. Install with: cabal install graphmod"; \
        exit 1; \
    fi
    # Generate module graph excluding external modules
    graphmod -i Data.Text,Data.Set,Control.Monad,System.IO \
        --no-cluster \
        -q lib/Halcyon lib/Halcyon.hs | dot -Tdot -o {{FILENAME}}
    echo "Module dependency graph saved to {{FILENAME}}"

# Install development tools needed for context generation
setup-context-tools:
    cabal install graphmod
    # Ensure dot (graphviz) is installed
    @if ! command -v dot &> /dev/null; then \
        echo "Graphviz not installed. Please install:"; \
        echo "  brew install graphviz  # on macOS"; \
        echo "  apt install graphviz   # on Ubuntu"; \
    fi