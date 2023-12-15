{ stdenv, bfc, brainfix, clang }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day15";
    srcs = [./src ./scripts];
    sourceRoot = ".";

    nativeBuildInputs = [
      bfc
      brainfix
      clang
    ];

    buildPhase = ''
      mkdir out

      bfc src/part1.bf
      mv part1 out

      bfx src/part2.bfx part2.bf
      bfc part2.bf
      mv part2 out
    '';

    installPhase = ''
      mkdir -p $out/{bin,libexec}
      cp out/part{1,2} $out/libexec
      cp scripts/sum $out/libexec

      cat <<EOF > $out/bin/day15
      #!/bin/bash
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$(basename "\$0") <path to input>"
        exit 1
      fi
      libexec="\$(dirname "\$0")/../libexec"

      echo "Part 1: \$("\$libexec/part1" < "\$1" | "\$libexec/sum")"
      echo "Part 2: \$("\$libexec/part2" < "\$1")"
      EOF

      chmod +x $out/bin/day15
    '';
  }
