{
  description = "Haskell C Compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        projectDependencies = with pkgs; [
          haskell.compiler.native-bignum.ghc9101        # GHC 9.10.1
          haskellPackages.cabal-install                 # 3.12.1.0
          haskellPackages.haskell-language-server       # 2.9.0.0
          haskellPackages.ghcid
          haskellPackages.hlint
        ];
        
        devDependencies = with pkgs; [
          llvmPackages.clang
          binutils
          qemu-user
          lldb
          nixpkgs-fmt
          python3
        ];
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = projectDependencies ++ devDependencies;
          shellHook = ''
            export NIX_CFLAGS_COMPILE="-march=x86-64"
            export PATH="$PWD/.cabal/bin:$PATH"
            export PATH="${pkgs.qemu-user}/bin:$PATH"
            export PATH="${pkgs.llvmPackages.clang}/bin:$PATH"
            export PATH="${pkgs.binutils}/bin:$PATH"
            export PATH="${pkgs.python3}/bin:$PATH"
            echo "Haskell Compiler Development Environment"
            echo "GHC Version: $(ghc --version)"
            echo "Cabal Version: $(cabal --version)"
            echo "Python Version: $(python3 --version)"
            echo "Ready to compile for x86_64 using QEMU"

            # Clone test suite if it doesn't exist
            if [ ! -d "writing-a-c-compiler-tests" ]; then
              git clone https://github.com/nlsandler/writing-a-c-compiler-tests.git
            fi
          '';
        };
      });
}
