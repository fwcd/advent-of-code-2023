{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day11";
    src = ./src;

    buildInputs = [
      pkgs.crystal
    ];

    buildPhase = ''
      crystal build day11.cr
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp day11 $out/bin
    '';
  }

