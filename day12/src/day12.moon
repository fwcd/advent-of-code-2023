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

solveImpl = (pattern, lengths, damaged, maxDamaged, i) ->
  if damaged > maxDamaged
    return 0
  if i <= #pattern
    -- TODO/IDEA: Look ahead n '?' and replace
    if string.sub(pattern, i, i) == '?'
      n = solveImpl (replace pattern, i, '.'), lengths, damaged, maxDamaged, i + 1
      m = solveImpl (replace pattern, i, '#'), lengths, damaged + 1, maxDamaged, i + 1
      n + m
    else
      solveImpl pattern, lengths, damaged, maxDamaged, i + 1
  elseif damaged == maxDamaged and isPotentialSolution pattern, lengths
    1
  else
    0

solve = (pattern, lengths) ->
  -- Multiple (fixed) dots can always be shortened to one, so let's simplify this
  pattern = string.gsub pattern, '%.+', '.'

  print "Solving #{pattern} with #{#lengths} groups"
  _, damaged = string.gsub pattern, '#', ''
  maxDamaged = sum lengths
  solveImpl pattern, lengths, damaged, maxDamaged, 0

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
