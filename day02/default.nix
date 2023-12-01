{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day02";
    src = ./src;

    buildInputs = [
      pkgs.gmp
      (pkgs.gnu-cobol.overrideAttrs (_: { doInstallCheck = false; })).bin
    ];

    buildPhase = ''
      mkdir -p out
      cobc -xo out/day02 day02.cob
      ls out
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day02 $out/bin
    '';
  }
