{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day06";
    src = ./src;

    buildInputs = [
      pkgs.llvm
    ];

    buildPhase = ''
      mkdir out

      llvm-as -o out/day06.bc day06.ll
      llc -o out/day06.o --filetype=obj out/day06.bc

      # We use the platform-specific C compiler driver to link the generated object
      # file. We could do this by hand invoking something along the lines of
      # 
      #     ld -lc <more platform-specific flags...> -o out/day06 out/day06.o
      #
      # It it, however, much more convenient to just use GCC/Clang's driver for this,
      # since we won't have to hardcode things like ld-linux, crt etc.

      $CC -lm -o out/day06 out/day06.o
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day06 $out/bin
    '';
  }
