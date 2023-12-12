parse = (line) ->
  -- Due to a compiler bug the destructured variables in the emitted lua code
  -- are declared in a sub-block where the outer scope cannot see them anymore,
  -- see https://github.com/leafo/moonscript/issues/449. To work around the
  -- issue, we declare them manually.
  local pattern, rawLengths
  {pattern, rawLengths} = [value for value in string.gmatch line, "%S+"]
  lengths = [tonumber rawLength for rawLength in string.gmatch rawLengths, "%d+"]
  return {pattern, lengths}

solve = (input) ->
  print input

if #arg < 1
  print "Usage: day12 <input>"
  os.exit 1

input = [parse line for line in io.lines arg[1] when line != ""]

for {pattern, lengths} in *input
  print pattern
  for length in *lengths
    print length
