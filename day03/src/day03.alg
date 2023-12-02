begin
  comment File 1 is stdout, file 2 is our input file;
  integer i, n;
  outstring(1, "Hello world!\n");
  for i := 0 step 1 until 2 do
  begin
    ininteger(2, n);
    outinteger(1, n);
  end
end
