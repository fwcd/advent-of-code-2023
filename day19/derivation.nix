{ stdenv, swift }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day19";
    src = ./src;

    nativeBuildInputs = [
      swift
    ];

    buildPhase = ''
      mkdir -p out
      swiftc -o out/day19 day19.swift
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day19 $out/bin
    '';
  }

