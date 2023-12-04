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
      replicate   = n: x: if n <= 0 then [] else [x] ++ replicate (n - 1) x;

      parseNums      = compose (map toInt) (filter isNonEmpty);
      parseSide      = compose parseNums (splitString " ");
      parseCard      = compose5 (c: { winning = head c; own = last c; }) (map parseSide) (splitString " | ") last (splitString ": ");
      cardMatchCount = c: length (intersectLists c.winning c.own);

      input       = readFile inputPath;
      lines       = splitString "\n" input;
      cards       = map parseCard lines;
      matchCounts = map cardMatchCount cards;

      values      = map (m: if m > 0 then pow 2 (m - 1) else 0) matchCounts;
      part1       = sum values;

      winCards    = n: times: counts: if n <= 0 || counts == [] then counts
                                                                else [(head counts + times)] ++ winCards (n - 1) times (tail counts);
      totalCounts = counts: matchCounts: if counts == [] then []
                                                         else let count      = head counts;
                                                                  matchCount = head matchCounts;
                                                              in [count] ++ totalCounts (winCards matchCount count (tail counts)) (tail matchCounts);

      part2       = sum (totalCounts (replicate (length cards) 1) matchCounts);

  in "Part 1: " + toString part1 + ", Part 2: " + toString part2
