{ inputPath }:
  let input = builtins.readFile inputPath;
  in input
