{ stdenv, fetchurl, ncurses }:
  stdenv.mkDerivation rec {
    name = "brainfix";
    version = "9fe6fa4831389a5bb88dd15461e6434bd31383dc";
    src = fetchurl {
      url = "https://github.com/jorenheit/brainfix/archive/${version}.tar.gz";
      sha256 = "64fa0e835e307b1e41df4b106e1da8efd4b1dd58fb84d8fcc1dc0f61f4e04136";
    };

    buildInputs = [
      ncurses
    ];

    buildPhase = ''
      make CC=$CXX
    '';

    installPhase = ''
      mkdir -p $out/{bin,share/bfx}
      cp bfx $out/bin
      cp -r std $out/share/bfx
    '';
  }
