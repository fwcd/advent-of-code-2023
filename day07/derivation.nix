{ stdenv, swiProlog }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day07";
    src = ./src;

    buildInputs = [
      swiProlog
    ];

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp day07.pl $out/share

      cat <<EOF > $out/bin/day07
      #!/bin/bash
      exec "${swiProlog.outPath}/bin/swipl" -s "\$(dirname "\$0")/../share/day07.pl" -t main --quiet -- "\$0" "\$@"
      EOF

      chmod +x $out/bin/day07
    '';
  }
