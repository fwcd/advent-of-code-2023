{ clangStdenv, swift, swiftPackages }:
  clangStdenv.mkDerivation {
    name = "advent-of-code-2023-day19";
    src = ./src;

    nativeBuildInputs = [
      swift
      swiftPackages.Foundation
    ];

    buildPhase = ''
      mkdir -p out

      # Explicitly set the target since swiftc seems to otherwise use an older
      # macOS version that doesn't support all of the regex stuff.
      target="$(swiftc --version 2>/dev/null | grep Target | sed 's/Target: //g')"
      swiftc -target "$target" -enable-bare-slash-regex -o out/day19 day19.swift
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp out/day19 $out/bin
    '';
  }

