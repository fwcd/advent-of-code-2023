#define ROWS 140
#define COLS 140

comment An implementation of day 3 in ALGOL 60 (GNU MARST)

        Resources I found helpful while learning enough ALGOL 60 to implement this:

        - https://www.gtoal.com/languages/algol60/marst-2.6/doc/marst.pdf
        - https://www.algol60.org/5shortIntro.htm
        - http://cs.ecs.baylor.edu/~maurer/SieveE/Algol60.htm
        - http://www.math.bas.bg/bantchev/place/rpn/rpn.algol60.html

        Our symbols are mapped to integers as follows:
        | 0 | ... | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
        | 0 | ... | 9 |  . |  * |  + |  $ |  # |  = |  @ |  / |  % |  & |  - |;

Boolean procedure isInBounds(i, j); value i, j; integer i, j;
  isInBounds := i >= 0 & i < ROWS & j >= 0 & j < COLS;

Boolean procedure isSymbol(cell); value cell; integer cell; isSymbol := cell > 10;
Boolean procedure isNumber(cell); value cell; integer cell; isNumber := cell < 10;
Boolean procedure isGear(cell);   value cell; integer cell; isGear   := cell = 11;

Boolean procedure neighbor(i, j, outNi, outNj, grid);
  value i, j, grid;
  integer i, j, outNi, outNj;
  integer array grid;
begin
  integer di, dj, ni, nj;
  for di := -1 step 1 until 1 do
    for dj := -1 step 1 until 1 do
      if di != 0 | dj != 0 then
      begin
        ni := i + di;
        nj := j + dj;
        if isInBounds(ni, nj) then
          if isSymbol(grid[ni, nj]) then
          begin
            outNi := ni;
            outNj := nj;
            neighbor := true;
            goto return
          end
      end;
  neighbor := false;
  return:
end;

procedure solve(outPart1, outPart2, grid);
  value grid;
  integer outPart1, outPart2;
  integer array grid;
begin
  integer i, j, ni, nj, number;
  integer array gears[0:ROWS, 0:COLS];
  Boolean hasNeighbor;

  procedure resetNumber;
  begin
    number := 0;
    hasNeighbor := false
  end;

  procedure consumeNumber;
  begin
    if number > 0 & hasNeighbor then
    begin
      outPart1 := outPart1 + number;
      if isGear(grid[ni, nj]) then
        if gears[ni, nj] = 0 then
          gears[ni, nj] := number
        else
          outPart2 := outPart2 + number * gears[ni, nj]
    end;
    resetNumber
  end;

  resetNumber;

  for i := 0 step 1 until ROWS - 1 do
    for j := 0 step 1 until COLS - 1 do
      gears[i, j] := 0;

  for i := 0 step 1 until ROWS - 1 do
  begin
    for j := 0 step 1 until COLS - 1 do
    begin
      if grid[i, j] < 10 then
      begin
        number := number * 10 + grid[i, j];
        if !hasNeighbor then
          if neighbor(i, j, ni, nj, grid) then
            hasNeighbor := true
      end
      else
        consumeNumber
    end;
    consumeNumber
  end
end;

begin
  integer i, j, cell, part1, part2;
  integer array grid[0:ROWS, 0:COLS];

  for i := 0 step 1 until ROWS - 1 do
  begin
    for j := 0 step 1 until COLS - 1 do
    begin
      comment File 2 is our input file;
      comment See the comment at the top for a description of this symbol
              mapping We need to specify % twice to escape it (presumably
              since it otherwise would be interpreted as a format specifier);

      inchar(2, "0123456789.*+$#=@/%%&-", cell);
      grid[i, j] := cell - 1
    end;
    inchar(2, "\n", cell)
  end;

  part1 := 0;
  part2 := 0;
  solve(part1, part2, grid);

  outstring(1, "Part 1: ");
  outinteger(1, part1);
  outstring(1, "\n");

  outstring(1, "Part 2: ");
  outinteger(1, part2);
  outstring(1, "\n")
end
