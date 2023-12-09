{ pkgs ? import <nixpkgs> {} }:
  pkgs.stdenv.mkDerivation rec {
    name = "pakcs";
    version = "3.6.0";
    src = pkgs.fetchurl {
      url = "https://www.informatik.uni-kiel.de/~pakcs/download/pakcs-${version}-src.tar.gz";
      sha256 = "d6bea3118dde10611229c01ea62ce255bc6922f40bb424ba974b6b054dd2186a";
    };

    buildInputs = [
      pkgs.gmp
      pkgs.libiconv
      pkgs.libffi
      pkgs.rlwrap
      pkgs.stack
      pkgs.swiProlog
      pkgs.which
    ];

    nativeBuildInputs = [];

    buildPhase = ''
      # Hack to make `security` (the Keychain CLI) visible from our Nix build,
      # otherwise Stack will complain.
      ${if pkgs.stdenv.isDarwin then ''
        mkdir tmp
        ln -s /usr/bin/security tmp/security
        export PATH="$PWD/tmp:$PATH"
      '' else ""}

      make \
        DISTPKGINSTALL=YES \
        CURRYLIBSDIR="$PWD/lib" \
        CURRYTOOLSDIR="$PWD/currytools" \
        PAKCSINSTALLDIR="$out"
    '';

    installPhase = ''
      for dir in bin lib src tools scripts currytools examples; do
        mkdir -p "$out/$dir"
        cp -r "$dir" "$out"
      done
    '';
  }
