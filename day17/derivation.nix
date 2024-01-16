{ stdenv, php }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day17";
    src = ./src;

    buildInputs = [
      php
    ];

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp day17.php $out/share

      cat <<EOF > $out/bin/day17
      #!/bin/bash
      exec "${php.outPath}/bin/php" -d memory_limit=1G "\$(dirname "\$0")/../share/day17.php" "\$@"
      EOF

      chmod +x $out/bin/day17
    '';
  }

