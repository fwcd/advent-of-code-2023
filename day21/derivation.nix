{ stdenv, dotnet-runtime, dotnet-sdk }:
  stdenv.mkDerivation {
    name = "advent-of-code-2023-day21";
    src = ./src;

    nativeBuildInputs = [
      dotnet-sdk
    ];

    buildInputs = [
      dotnet-runtime
    ];

    # https://stackoverflow.com/questions/46065777/is-it-possible-to-compile-a-single-c-sharp-code-file-with-the-net-core-roslyn-c

    buildPhase = ''
      sdk="$(echo "${dotnet-sdk.outPath}"/sdk/*)"
      shared="$(echo "${dotnet-runtime.outPath}"/shared/Microsoft.NETCore.App/*)"
      dotnet "$sdk/Roslyn/bincore/csc.dll" \
        -r:"$shared/System.Private.CoreLib.dll" \
        -r:"$shared/System.Runtime.dll" \
        -r:"$shared/System.Console.dll" \
        day21.cs
    '';

    installPhase = ''
      mkdir -p $out/{bin,lib}
      cp day21.exe $out/lib

      cat <<EOF > $out/lib/day21.runtimeconfig.json
      {
        "runtimeOptions": {
          "framework": {
            "name": "Microsoft.NETCore.App",
            "version": "$(ls ${dotnet-runtime.outPath}/shared/Microsoft.NETCore.App)"
          }
        }
      }
      EOF

      cat <<EOF > $out/bin/day21
      #!/bin/bash
      exec "${dotnet-runtime.outPath}/bin/dotnet" "\$(dirname "\$0")/../lib/day21.exe" "\$@"
      EOF

      chmod +x $out/bin/day21
    '';
  }

