{ stdenv, php }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day18";
    src = ./src;

    buildPhase = ''
      mkdir -p out
      $CXX -std=c++17 -o out/day18 day18.cpp
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day18 $out/bin
    '';
  }

