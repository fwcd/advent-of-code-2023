{
  pkgs ? import <nixpkgs> {},
  pakcs ? import ./pakcs {}
}:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day09";
    src = ./src;

    buildInputs = [
      pakcs
    ];

    buildPhase = ''
      # TODO: Cleanup
      export HOME="$(mktemp -d)"
      pakcs :load Day09 :save main :quit
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp Day09 $out/bin/day09
    '';
  }
