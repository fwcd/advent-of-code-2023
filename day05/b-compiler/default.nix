# This is the B compiler we are using: https://cpjsmith.uk/b
# It compiles B programs to a bytecode that closely mirrors Ken Thompson's
# reference machine and ships an interpreter for this bytecode.
{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation rec {
    name = "b-compiler";
    version = "1.1";
    src = pkgs.fetchurl {
      url = "https://cpjsmith.uk/downloads/b/b-${version}.tar.gz";
      sha256 = "bd1ff5ca12dbfa82ffe966ac248f9c939f4ca5493734f6dc836679aa49a9cb1a";
    };

    nativeBuildInputs = [
      pkgs.m4
      pkgs.flex
      pkgs.bison
    ];

    patches = [
      patches/fix-cerr-endl-typo.patch
      patches/std-cxx-03.patch
      patches/fix-undeclared-yyscanner-type.patch
    ];

    # The tarball does not contain a 'top-level directory', therefore we'll have to do this
    sourceRoot = ".";

    # Enable larger word size and extensions such as the `break` keyword to make
    # our life a bit easier.
    configurePhase = ''
      ./reconfigure --word_size 64 --extensions
    '';

    buildPhase = ''
      make CXX=$CXX
    '';

    installPhase = ''
      mkdir -p $out/{bin,share/b}

      cp {bc,ba,br} $out/bin
      cp libb.ba $out/share/b
    '';

    meta = {
      description = "A bytecode compiler for the B programming language";
      homepage = "https://cpjsmith.uk/b";
    };
  }
