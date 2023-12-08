{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day08";
    src = ./src;

    buildInputs = if pkgs.stdenv.isDarwin then [
      pkgs.darwin.apple_sdk.frameworks.Foundation
    ] else [
      pkgs.clang
      pkgs.gnustep.base
    ];

    buildPhase = ''
      mkdir -p out
      clang -framework Foundation -o out/day08 day08.m
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day08 $out/bin
    '';
  }
