{ inputPath, lib ? import <nixpkgs/lib> }:
  let input = lib.strings.splitString "\n" (builtins.readFile inputPath);
  in input
