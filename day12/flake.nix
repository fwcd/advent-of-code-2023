{
  description = "Advent of Code - Day 12 solution";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    in {
      packages = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          default = pkgs.stdenv.mkDerivation {
            name = "advent-of-code-2023-day12";
            src = ./src;

            buildInputs = [
              pkgs.luajit
            ];

            nativeBuildInputs = [
              pkgs.luajitPackages.moonscript
            ];

            buildPhase = ''
              mkdir out
              moonc -o out/day12.lua day12.moon
              luajit -bg out/day12.lua out/day12.ljbc
            '';

            installPhase = ''
              mkdir -p $out/{bin,share,src}
              cp day12.moon out/day12.lua $out/src
              cp out/day12.ljbc $out/share

              cat <<EOF > $out/bin/day12
              #!/bin/bash
              exec "${pkgs.luajit.outPath}/bin/luajit" "\$(dirname "\$0")/../share/day12.ljbc" "\$@"
              EOF

              chmod +x $out/bin/day12
            '';
          };
        }
      );

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/day12";
        };
      });
    };
}
