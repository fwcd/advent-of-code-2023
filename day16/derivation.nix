{ stdenv, zig }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day16";
    src = ./src;
    sourceRoot = ".";

    nativeBuildInputs = [
      zig
    ];

    buildPhase = ''
      mkdir out cache

      zig build-exe --global-cache-dir cache -femit-bin=out/day16 src/day16.zig
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day16 $out/bin
    '';
  }
