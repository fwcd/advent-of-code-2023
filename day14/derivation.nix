{ stdenv, go }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day14";
    src = ./src;
    sourceRoot = ".";

    nativeBuildInputs = [
      go
    ];

    buildPhase = ''
      mkdir out cache

      GOCACHE=$PWD/cache go build -o out/day14 src/day14.go
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day14 $out/bin
    '';
  }
