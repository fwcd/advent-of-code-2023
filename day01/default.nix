{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day01";
    src = ./src;

    buildPhase = ''
      mkdir -p out
      $CC -o out/day01 day01.c
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day01 $out/bin
    '';
  }
