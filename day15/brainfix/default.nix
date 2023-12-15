{ stdenv, fetchurl }:
  stdenv.mkDerivation rec {
    name = "brainfix";
    version = "0.42";
    src = fetchurl {
      url = "https://downloads.sourceforge.net/project/brainfix/bfx_v${version}.tar.gz";
      sha256 = "ea0abdda03814b4c3bbc1bc34735f16465f7523f187f0bcb8db089a722ff6e51";
    };

    # The tarball does not contain a 'top-level directory', therefore we'll have to do this
    sourceRoot = ".";

    buildPhase = ''
      make CC=$CXX
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp bfx $out/bin
    '';
  }
