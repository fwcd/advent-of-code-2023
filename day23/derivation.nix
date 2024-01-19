{ stdenv, ruby }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day23";
    src = ./src;

    buildInputs = [
      ruby
    ];

    installPhase = ''
      mkdir -p $out/bin
      cp day23.rb $out/bin/day23
      chmod +x $out/bin/day23
    '';
  }

