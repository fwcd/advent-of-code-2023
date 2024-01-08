{ stdenv, fetchurl }:
  stdenv.mkDerivation rec {
    name = "dmd";
    version = "2.106.1";
    src = fetchurl {
      url = "https://downloads.dlang.org/releases/2.x/${version}/dmd.${version}.${if stdenv.isDarwin then "osx" else "linux"}.tar.xz";
      sha256 = if stdenv.isDarwin then
        "b1e54ef9396a50be22aa6651ba6927ca96ba4e6e5c3f09959b8df973c60a2c87"
      else
        "706c11fc80aa5c25a6b26093bebc51914050e58335791badf5181a6dd8764287"
      ;
    };

    installPhase = if stdenv.isDarwin then ''
      mkdir -p "$out"
      cp -r osx/{bin,lib} "$out"
    '' else ''
      mkdir -p "$out"
      cp -r linux/bin64 "$out/bin"
      cp -r linux/lib64 "$out/lib"
    '';
  }
