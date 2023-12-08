{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day04";
    src = ./src;

    buildInputs = [
      pkgs.coreutils # for realpath
    ];

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp day04.nix $out/share
      
      cat <<EOF > $out/bin/day04
      #!/bin/bash
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$0 <path to input>"
        exit 1
      fi
      exec nix-instantiate --eval --strict --arg inputPath "\"\$(${pkgs.coreutils.outPath}/bin/realpath \$1)\"" "\$(dirname "\$0")/../share/day04.nix"
      EOF

      chmod +x $out/bin/day04
    '';
  }
