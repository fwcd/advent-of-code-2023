{ stdenv, glib, pkg-config, vala }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day13";
    src = ./src;
    sourceRoot = ".";

    nativeBuildInputs = [
      pkg-config
      vala
    ];

    buildInputs = [
      glib
    ];

    buildPhase = ''
      mkdir out

      valac --pkg gio-2.0 -o out/day13 src/day13.vala
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day13 $out/bin
    '';
  }
