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

      cp src/part1.bf out
      (cd out && bfc part1.bf)

      # Run the C preprocessor and make sure lines align by
      # replacing preprocessor directives with empty lines.
      cat src/part2.bfx | cpp | awk '!/^#/' > out/part2.preprocessed.bfx

      bfx -o out/part2.bf -I ${brainfix.outPath}/share/bfx/std out/part2.preprocessed.bfx
      (cd out && bfc part2.bf)
    '';

    installPhase = ''
      mkdir -p $out/{bin,libexec,src}
      cp out/part{1,2} $out/libexec
      cp scripts/sum $out/libexec
      cp src/part1.bf out/part2.bf $out/src

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
