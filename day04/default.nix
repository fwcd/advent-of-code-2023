{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day04";
    src = ./src;

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp day04.nix $out/share
      
      cat <<EOF > $out/bin/day04
      #!/bin/bash
      nix-instantiate --eval --arg x 4 --arg y 3 $out/share/day04.nix
      EOF

      chmod +x $out/bin/day04
    '';
  }
