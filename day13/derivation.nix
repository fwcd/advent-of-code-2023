{ stdenv, vala }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day13";
    src = ./src;
    sourceRoot = ".";

    nativeBuildInputs = [
      vala
    ];

    buildPhase = ''
      mkdir out

      valac -o out/day13 src/day13.vala
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day13 $out/bin
    '';
  }
