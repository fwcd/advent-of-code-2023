{ stdenv, ghc }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day24";
    src = ./src;

    nativeBuildInputs = [
      ghc
    ];

    buildPhase = ''
      mkdir out
      ghc -o out/day24 Day24.hs
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day24 $out/bin
    '';
  }

