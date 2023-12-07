{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day07";
    src = ./src;

    buildInputs = [
      pkgs.swiProlog
    ];

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp day07.pl $out/share

      cat <<EOF > $out/bin/day07
      #!/bin/bash
      exec ${pkgs.swiProlog.outPath}/bin/swipl -s "\$(dirname "\$0")/../share/day07.pl" -t main --quiet -- "\$0" "\$@"
      EOF

      chmod +x $out/bin/day07
    '';
  }
