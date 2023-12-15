{ stdenv, bfc, clang }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day15";
    srcs = [./src ./scripts];
    sourceRoot = ".";

    nativeBuildInputs = [
      bfc
      clang
    ];

    buildPhase = ''
      mkdir out

      bfc src/part1.bf
      mv part1 out
    '';

    installPhase = ''
      mkdir -p $out/{bin,libexec}
      cp out/part1 $out/libexec/part1-impl
      cp scripts/sum $out/libexec

      cat <<EOF > $out/bin/day15
      #!/bin/bash
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$(basename "\$0") <path to input>"
        exit 1
      fi
      libexec="\$(dirname "\$0")/../libexec"
      echo "Part 1: \$("\$libexec/part1-impl" < "\$1" | "\$libexec/sum")"
      EOF

      chmod +x $out/bin/day15
    '';
  }
