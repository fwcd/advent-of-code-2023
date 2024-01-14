{ stdenv, kotlin-native }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day20";
    src = ./src;

    nativeBuildInputs = [
      kotlin-native
    ];

    buildPhase = ''
      mkdir -p .konan out
      KONAN_DATA_DIR=$PWD/.konan kotlinc-native -o out/day20 day20.kt
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day20.kexe $out/bin/day20
    '';
  }

