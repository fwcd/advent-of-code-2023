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
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$0 <path to input>"
        exit 1
      fi
      if [ ! -f "\$1" ]; then
        echo "File \$1 does not exist!"
        exit 1
      fi
      exec php "\$(dirname "\$0")/../share/day17.php"
      EOF

      chmod +x $out/bin/day17
    '';
  }

