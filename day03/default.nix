{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day03";
    src = ./src;

    buildInputs = [
      pkgs.marst
    ];

    buildPhase = ''
      mkdir -p out

      # Run the C preprocessor and make sure lines align by
      # replacing preprocessor directives with empty lines.
      cat day03.alg | cpp | awk '!/^#/' > out/day03.preprocessed.alg

      marst -o out/day03.c out/day03.preprocessed.alg
      cc -o out/day03-impl -lalgol -lm out/day03.c
    '';

    installPhase = ''
      mkdir -p $out/{libexec,bin,src}
      cp out/day03-impl $out/libexec
      cp out/*.{c,alg} $out/src

      cat <<EOF > $out/bin/day03
      #!/bin/bash
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$0 <path to input>"
        exit 1
      fi
      if [ ! -f "\$1" ]; then
        echo "File \$1 does not exist!"
        exit 1
      fi
      FILE_2="\$1" "\$(dirname "\$0")/../libexec/day03-impl"
      EOF

      chmod +x $out/bin/day03
    '';
  }
