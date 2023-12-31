{ stdenv, bfc, clang, ldc }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day15";
    srcs = [./src ./scripts];
    sourceRoot = ".";

    nativeBuildInputs = [
      bfc
      clang
      ldc
    ];

    buildPhase = ''
      mkdir out

      cp src/part1.bf out
      (cd out && bfc part1.bf)

      ldc2 -of=out/part2 src/part2.d
    '';

    installPhase = ''
      mkdir -p $out/{bin,libexec,src}
      cp out/part{1,2} $out/libexec
      cp scripts/sum $out/libexec
      cp src/part1.bf $out/src

      cat <<EOF > $out/bin/day15
      #!/bin/bash
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$(basename "\$0") <path to input>"
        exit 1
      fi
      libexec="\$(dirname "\$0")/../libexec"

      echo "Part 1: \$("\$libexec/part1" < "\$1" | "\$libexec/sum")"
      echo "Part 2: \$("\$libexec/part2" "\$1")"
      EOF

      chmod +x $out/bin/day15
    '';
  }
