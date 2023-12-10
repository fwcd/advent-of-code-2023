{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation {
    name = "advent-of-code-2023-day10";
    src = ./src;

    buildInputs = [
      pkgs.jdk21
    ];

    buildPhase = ''
      javac Day10.java
    '';

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp Day10.class $out/share

      cat <<EOF > $out/bin/day10
      #!/bin/sh
      "${pkgs.jdk21.outPath}/bin/java" -classpath "\$(dirname "\$0")/../share" Day10
      EOF

      chmod +x $out/bin/day10
    '';
  }
