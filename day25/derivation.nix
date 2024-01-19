{ stdenv, python3 }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day25";
    src = ./src;

    buildInputs = [
      python3
    ];

    installPhase = ''
      mkdir -p $out/bin
      cp day25.py $out/bin/day25
      chmod +x $out/bin/day25
    '';
  }

