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
      $CC -o out/day06 out/day06.o
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day06 $out/bin
    '';
  }