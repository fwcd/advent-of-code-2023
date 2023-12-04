{ inputPath, lib ? import <nixpkgs/lib> }:
  let inherit (builtins) elem foldl' filter length readFile stringLength;
      inherit (lib.strings) splitString toInt;
      inherit (lib.lists) head tail last intersectLists reverseList take;
      inherit (lib.trivial) flip pipe;
  in
  let compose     = f: g: x: f (g x);
      compose3    = f: compose (compose compose compose f) compose; # = f: compose (compose (compose f)) compose
                                                                    # = f: g: compose (compose (compose f)) compose g
                                                                    # = f: g: compose (compose f) (compose g)
                                                                    # = f: g: h: compose (compose f) (compose g) h
                                                                    # = f: g: h: compose f (compose g h)
                                                                    # = f: g: h: x: compose f (compose g h) x
                                                                    # = f: g: h: x: f ((compose g h) x)
                                                                    # = f: g: h: x: f (g (h x))
      compose4    = f: g: h: i: compose f (compose3 g h i);
      compose5    = f: g: h: i: j: compose f (compose4 g h i j);
      isNonEmpty  = s: stringLength s > 0;
      sum         = foldl' (x: y: x + y) 0;
      pow         = n: m: if m == 0 then 1 else n * (pow n (m - 1));
      cons        = x: xs: [x] ++ xs;

      parseNums   = compose (map toInt) (filter isNonEmpty);
      parseSide   = compose parseNums (splitString " ");
      parseCard   = compose5 (c: { winning = head c; own = last c; }) (map parseSide) (splitString " | ") last (splitString ": ");
      cardValue   = c: let count = length (intersectLists c.winning c.own);
                       in if count > 0 then pow 2 (count - 1) else 0; # TODO: Implement the actual function

      input  = readFile inputPath;
      lines  = splitString "\n" input;
      cards  = map parseCard lines;
      values = map cardValue cards;
      
      computePart2  = acc: remaining: if remaining == [] then sum acc - 1
                                                         else let wonCards = 1 + sum (take (head remaining) acc);
                                                              in computePart2 (builtins.trace (toString acc) (cons wonCards acc)) (tail remaining);

      part1  = sum values;
      part2  = computePart2 [] (reverseList values);
  in "Part 1: " + toString part1 + ", Part 2: " + toString part2
