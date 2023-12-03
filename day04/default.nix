{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day04";
    src = ./src;

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
      nix-instantiate --eval --expr "import $out/share/day04.nix { inputPath = \"\$(realpath \$1)\"; }"
      EOF

      chmod +x $out/bin/day04
    '';
  }
