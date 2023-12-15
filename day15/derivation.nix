{ stdenv, bfc, clang }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day15";
    src = ./src;

    nativeBuildInputs = [
      bfc
      clang
    ];

    buildPhase = ''
      mkdir out
      bfc day15.bf
    '';

    installPhase = ''
      mkdir -p $out/{bin,libexec}
      cp day15 $out/libexec/day15-impl

      cat <<EOF > $out/bin/day15
      #!/bin/bash
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$(basename "\$0") <path to input>"
        exit 1
      fi
      exec "\$(dirname "\$0")/../libexec/day15-impl" < "\$1"
      EOF

      chmod +x $out/bin/day15
    '';
  }
