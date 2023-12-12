{ stdenv, jdk21 }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day10";
    src = ./src;

    buildInputs = [
      jdk21
    ];

    buildPhase = ''
      javac Day10.java
    '';

    installPhase = ''
      mkdir -p $out/{bin,share}
      cp *.class $out/share

      cat <<EOF > $out/bin/day10
      #!/bin/sh
      "${jdk21.outPath}/bin/java" -classpath "\$(dirname "\$0")/../share" Day10 "\$@"
      EOF

      chmod +x $out/bin/day10
    '';
  }
