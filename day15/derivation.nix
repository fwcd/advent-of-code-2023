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
      (cd src && bfc day15.bf)
      mv src/day15 out
    '';

    installPhase = ''
      mkdir -p $out/{bin,libexec}
      cp out/day15 $out/libexec/day15-impl
      cp scripts/sum $out/libexec/day15-sum

      cat <<EOF > $out/bin/day15
      #!/bin/bash
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$(basename "\$0") <path to input>"
        exit 1
      fi
      libexec="\$(dirname "\$0")/../libexec"
      exec "\$libexec/day15-impl" < "\$1" | "\$libexec/day15-sum"
      EOF

      chmod +x $out/bin/day15
    '';
  }
