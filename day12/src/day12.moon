parse = (line) ->
  -- Due to a compiler bug the destructured variables in the emitted lua code
  -- are declared in a sub-block where the outer scope cannot see them anymore,
  -- see https://github.com/leafo/moonscript/issues/449. To work around the
  -- issue, we declare them manually.
  local pattern, rawLengths
  {pattern, rawLengths} = [value for value in string.gmatch line, '%S+']
  lengths = [tonumber rawLength for rawLength in string.gmatch rawLengths, '%d+']
  {pattern, lengths}

sum = (values) -> 
  result = 0
  for value in *values
    result += value
  result

prefixLength = (str, i, repeated) -> 
  rest = string.sub str, i
  low, high = string.find rest, "^#{repeated}*"
  high - low + 1

replace = (str, i, n, replacement) ->
  "#{string.sub(str, 1, i - 1)}#{replacement}#{string.sub(str, i + n)}"

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

solveImpl = (pattern, lengths, i) ->
  if #lengths == 0
    return 1
  if i > #pattern
    return 0

  nextLength = lengths[1]
  remLengths = [length for length in *lengths[2,]]
  c = string.sub pattern, i, i
  solutions = 0

  if c != '#'
    -- Skip the spot (i.e. replacing this ? with .)
    solutions += solveImpl pattern, lengths, i + 1
  if c != '.'
    completable = prefixLength pattern, i, '[#?]'
    -- ...or insert the next group, i.e. replacing nextLength ?s with # if
    -- there are no .s in the way
    if completable >= nextLength
      nextI = i + nextLength
      if string.sub(pattern, nextI, nextI) == '#'
        -- This would force a too long group
        switch c
          when '?'
            -- If we were on an unknown, we can continue by matching from that #
            -- (conceptually replacing all intermediate ?s with .)...
            solutions += solveImpl pattern, lengths, nextI
          when '#'
            -- Since this group was already fixed, we are forced to take it, so
            -- we've hit a dead end.
            return 0
      else
        -- The group matches, conceptually place a . thereafter and continue...
        solutions += solveImpl pattern, remLengths, nextI + 1

  solutions

solve = (pattern, lengths) ->
  -- Multiple (fixed) dots can always be shortened to one, so let's simplify this
  pattern = string.gsub pattern, '%.+', '.'

  print "Solving #{pattern} with #{#lengths} groups"
  solutions = solveImpl pattern, lengths, 1
  print "  -> #{solutions}"
  solutions

if #arg < 1
  print 'Usage: day12 <input>'
  os.exit 1

input = [parse line for line in io.lines arg[1] when line != '']
part1 = 0
part2 = 0

for {pattern, lengths} in *input
  part1 += solve pattern, lengths

  longPattern = "#{pattern}?#{pattern}?#{pattern}?#{pattern}?#{pattern}"
  longLengths = [length for i = 1, 5 for length in *lengths]
  -- TODO: part2 += solve longPattern, longLengths

print "Part 1: #{part1}"
print "Part 2: #{part2}"
