{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day03";
    src = ./src;

    buildInputs = [
      pkgs.marst
    ];

    buildPhase = ''
      mkdir -p out
      marst -o out/day03.c day03.alg
      cc -o out/day03-impl -lalgol -lm out/day03.c
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day03-impl $out/bin

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
      installdir="\$PWD/\$(dirname "\$0")"
      tmpdir="\$(mktemp -d)"
      trap "rm -rf -- '\$tmpdir'" EXIT
      cat "\$1" > "\$tmpdir/FILE_2"
      cd "\$tmpdir"
      "\$installdir/day03-impl"
      EOF

      chmod +x $out/bin/day03
    '';
  }
