{
  pkgs ? import <nixpkgs> {},
  b-compiler ? import ./b-compiler {}
}:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day05";
    src = ./src;

    buildInputs = [
      b-compiler
    ];

    buildPhase = ''
      mkdir out
      bc -o out/day05.ba day05.b
      ba -o out/day05.bin ${b-compiler.outPath}/share/b/libb.ba out/day05.ba
    '';

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp out/day05.bin $out/share

      cat <<EOF > $out/bin/day05
      #!/bin/bash
      set -e
      if [ -z "\$1" ]; then
        echo "Usage: \$0 <path to input>"
        exit 1
      fi
      ${b-compiler.outPath}/bin/br "\$(dirname "\$0")/../share/day05.bin" < "\$1"
      EOF

      chmod +x $out/bin/day05
    '';
  }