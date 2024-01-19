{ stdenv, rustc }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day22";
    src = ./src;

    nativeBuildInputs = [
      rustc
    ];

    buildPhase = ''
      mkdir -p out
      rustc -o out/day22 day22.rs
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day22 $out/bin/day22
    '';
  }

