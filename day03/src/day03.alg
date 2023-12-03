#define ROWS 140
#define COLS 140

comment An implementation of day 3 in ALGOL 60 (GNU MARST);

comment Our symbols are mapped to integers as follows:;
comment | 0 | ... | 9 | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |;
comment | 0 | ... | 9 |  . |  * |  + |  $ |  # |  = |  @ |  / |  % |  & |  - |;

Boolean procedure isInBounds(i, j); integer i, j; isInBounds := i >= 0 & i < ROWS & j >= 0 & j < COLS;
Boolean procedure isSymbol(cell); integer cell; isSymbol := cell > 10;
Boolean procedure isNumber(cell); integer cell; isNumber := cell < 10;

Boolean procedure hasNeighboringSymbol(i, j, grid);
  integer i, j;
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
        begin
          if grid[ni, nj] > 10 then
          begin
            hasNeighboringSymbol := true;
            goto return;
          end;
        end;
      end;
  hasNeighboringSymbol := false;
  return:
end;

integer procedure part1(grid);
  integer array grid;
begin
  integer i, j, number, result;
  Boolean hasNeighbor;

  procedure consumeNumber;
  begin
    if number > 0 & hasNeighbor then
      result := result + number;
    number := 0;
    hasNeighbor := false;
  end;

  number := 0;
  result := 0;
  hasNeighbor := false;

  for i := 0 step 1 until ROWS - 1 do
  begin
    for j := 0 step 1 until COLS - 1 do
    begin
      if grid[i, j] < 10 then
      begin
        number := number * 10 + grid[i, j];
        hasNeighbor := hasNeighbor | hasNeighboringSymbol(i, j, grid);
      end else
        consumeNumber;
    end;
    consumeNumber;
  end;
  part1 := result;
end;

begin
  integer i, j, cell;
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
      grid[i, j] := cell - 1;
    end;
    inchar(2, "\n", cell);
  end;

  outstring(1, "Part 1: ");
  outinteger(1, part1(grid));
  outstring(1, "\n");
end
