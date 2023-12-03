#define ROWS 10
#define COLS 10

comment An implementation of day 3 in ALGOL 60 (GNU MARST);

Boolean procedure isInBounds(i, j);
  integer i, j;
begin
  isInBounds := i >= 0 & i < ROWS & j >= 0 & j < COLS;
end;

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
          if grid[ni, nj] > 10 then
          begin
            hasNeighboringSymbol := true;
            goto return;
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
    if number > 0 then
    begin
      if hasNeighbor then
        result := result + number;
      print(number);
      number := 0;
      hasNeighbor := false;
    end;
  end;

  number := 0;
  result := 0;
  hasNeighbor := false;

  for i := 0 step 1 until ROWS - 1 do
  begin
    for j := 0 step 1 until COLS - 1 do
    begin
      if grid[i, j] < 10 then
        number := number * 10 + grid[i, j]
      else
        consumeNumber;
      hasNeighbor := hasNeighbor | hasNeighboringSymbol(i, j, grid);
    end;
    consumeNumber;
  end;
  part1 := result;
end;

begin
  integer i, j, cell;
  integer array grid[0:ROWS, 0:COLS];

  comment File 1 is stdout, file 2 is our input file;

  for i := 0 step 1 until ROWS - 1 do
  begin
    for j := 0 step 1 until COLS - 1 do
    begin
      comment Our symbols are mapped to integers as follows:;
      comment | 0 | ... | 9 | 10 | 11 | 12 | 13 | 14 |;
      comment | 0 | ... | 9 |  . |  * |  + |  $ |  # |;

      inchar(2, "123456789.*+$#", cell);
      grid[i, j] := cell;
    end;
    inchar(2, "\n", cell);
  end;

  print(part1(grid));
end
