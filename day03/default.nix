{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day03";
    src = ./src;

    buildInputs = [
      pkgs.marst
    ];

    buildPhase = ''
      mkdir -p out
      marst -o out/day03.c day03.alg
      cc -o out/day03 -lalgol -lm out/day03.c
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day03 $out/bin
    '';
  }
