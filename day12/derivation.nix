{ stdenv, luajit, luajitPackages }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day12";
    src = ./src;

    buildInputs = [
      luajit
    ];

    nativeBuildInputs = [
      luajitPackages.moonscript
    ];

    buildPhase = ''
      mkdir out
      moonc -o out/day12.lua day12.moon
      luajit -bg out/day12.lua out/day12.ljbc
    '';

    installPhase = ''
      mkdir -p $out/{bin,share,src}
      cp day12.moon out/day12.lua $out/src
      cp out/day12.ljbc $out/share

      cat <<EOF > $out/bin/day12
      #!/bin/bash
      exec "${luajit.outPath}/bin/luajit" "\$(dirname "\$0")/../share/day12.ljbc" "\$@"
      EOF

      chmod +x $out/bin/day12
    '';
  }
