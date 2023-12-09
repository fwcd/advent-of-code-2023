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
    ] ++ (if pkgs.stdenv.isDarwin then [] else [
      pkgs.glibcLocales
    ]);

    nativeBuildInputs = [];

    patchPhase = ''
      # Enable more verbose stack output
      sed -i"" -e 's/\(\$(STACK) install\)/\1 --cabal-verbose/g' frontend/Makefile

      # Patch out rl_add_history call which seems to be unavailable
      sed -i"" -e 's/rl_add_history([^)]*)/true/g' src/swibasics.pl

      # Patch out CurryHtml module, something with the use of TemplateHaskell is
      # broken. Since we do not need to generate documentation and the issue
      # seems to be hard to debug, we'll patch out this module.
      rm frontend/src/Html/CurryHtml.hs
      sed -i"" -e 's/, Html.CurryHtml//g' frontend/curry-frontend.cabal
      sed -i"" -e 's/import Html.CurryHtml .*//g' frontend/src/Modules.hs
      sed -i"" -e 's/  source2html .*/  error "HTML generation is patched out in this build!"/g' frontend/src/Modules.hs
    '';

    buildPhase = ''
      ${if pkgs.stdenv.isDarwin then ''
        # Hack to make `security` (the Keychain CLI) visible from our Nix build,
        # otherwise Stack will complain.
        mkdir tmp
        ln -s /usr/bin/security tmp/security
        export PATH="$PWD/tmp:$PATH"
      '' else ''
        export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
      ''}

      export LC_ALL=en_US.UTF-8

      make \
        DISTPKGINSTALL=YES \
        CURRYLIBSDIR="$PWD/lib" \
        CURRYTOOLSDIR="$PWD/currytools" \
        PAKCSINSTALLDIR="$out"
    '';

    installPhase = ''
      for dir in bin lib src frontend tools scripts currytools examples; do
        mkdir -p "$out/$dir"
        cp -r "$dir" "$out"
      done

      cp pakcsrc.default "$out"
    '';
  }
