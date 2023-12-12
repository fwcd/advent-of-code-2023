parse = (line) ->
  -- Due to a compiler bug the destructured variables in the emitted lua code
  -- are declared in a sub-block where the outer scope cannot see them anymore,
  -- see https://github.com/leafo/moonscript/issues/449. To work around the
  -- issue, we declare them manually.
  local pattern, rawLengths
  {pattern, rawLengths} = [value for value in string.gmatch line, '%S+']
  lengths = [tonumber rawLength for rawLength in string.gmatch rawLengths, '%d+']
  {pattern, lengths}

replace = (str, i, replacement) ->
  "#{string.sub(str, 1, i - 1)}#{replacement}#{string.sub(str, i + 1)}"

isPotentialSolution = (pattern, lengths) ->
  length = 0
  reverseLengths = [lengths[#lengths - i] for i = 0, #lengths - 1]
  popLengthExpected = ->
    expected = table.remove reverseLengths, #reverseLengths
    length == expected

  for c in string.gmatch pattern, '.'
    switch c
      when '#'
        length += 1
      when '.'
        if length > 0
          if not popLengthExpected!
            return false
        length = 0
      when '?'
        -- We accept a superset of all solutions since we'd have to search both
        -- choices here to accurately continue (which we already do in the
        -- solver).
        return true
  if length > 0
    if not popLengthExpected!
      return false
  #reverseLengths == 0

solve = (pattern, lengths, i = 0) ->
  if i <= #pattern
    if string.sub(pattern, i, i) == '?'
      n = solve (replace pattern, i, '.'), lengths, i + 1
      m = solve (replace pattern, i, '#'), lengths, i + 1
      n + m
    else
      solve pattern, lengths, i + 1
  elseif isPotentialSolution pattern, lengths
    1
  else
    0

if #arg < 1
  print 'Usage: day12 <input>'
  os.exit 1

input = [parse line for line in io.lines arg[1] when line != '']

for {pattern, lengths} in *input
  print "Solving #{pattern}"
  n = solve pattern, lengths
  print n
