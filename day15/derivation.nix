{ stdenv, bfc }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day15";
    src = ./src;

    buildInputs = [
      bfc
    ];

    buildPhase = ''
      mkdir out
      bfc day15.bf
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp day15 $out/bin
    '';
  }
