parse = (line) ->
  -- Due to a compiler bug the destructured variables in the emitted lua code
  -- are declared in a sub-block where the outer scope cannot see them anymore,
  -- see https://github.com/leafo/moonscript/issues/449. To work around the
  -- issue, we declare them manually.
  local pattern, rawLengths
  {pattern, rawLengths} = [value for value in string.gmatch line, '%S+']
  lengths = [tonumber rawLength for rawLength in string.gmatch rawLengths, '%d+']
  {pattern, lengths}

prefixLength = (str, i, repeated) -> 
  rest = string.sub str, i
  low, high = string.find rest, "^#{repeated}*"
  high - low + 1

memoized = {}
hit = 0
miss = 0

solveImpl = (pattern, lengths, i) ->
  memoize = false
  local memoizationKey

  memoizationKey = table.concat {string.sub(pattern, i), table.concat(lengths, ',')}, ';'
  if memoized[memoizationKey]
    hit += 1
    return memoized[memoizationKey]
  miss += 1
  memoize = true

  if #lengths == 0
    if i > #pattern or prefixLength(pattern, i, '[^#]') == #pattern - i + 1
      return 1
    else
      return 0
  if i > #pattern
    return 0

  nextLength = lengths[1]
  remLengths = [length for length in *lengths[2,]]
  c = string.sub pattern, i, i
  solutions = 0

  if c != '#'
    -- Skip forward (i.e. replacing this ? with .)
    skip = 1 + prefixLength(pattern, i + 1, '%.')
    solutions += solveImpl pattern, lengths, i + skip
  if c != '.'
    completable = prefixLength pattern, i, '[#?]'
    -- ...or insert the next group, i.e. replacing nextLength ?s with # if
    -- there are no .s in the way
    if completable >= nextLength
      nextI = i + nextLength
      if string.sub(pattern, nextI, nextI) == '#'
        -- This would force a too long group
        if c == '#'
          -- Since this group was already fixed, we are forced to take it, so
          -- we've hit a dead end.
          return 0
      else
        -- The group matches, conceptually place a . thereafter and continue...
        solutions += solveImpl pattern, remLengths, nextI + 1

  if memoize
    memoized[memoizationKey] = solutions

  solutions

solve = (pattern, lengths) ->
  -- Multiple (fixed) dots can always be shortened to one, so let's simplify this
  pattern = string.gsub pattern, '%.+', '.'
  solveImpl pattern, lengths, 1, ''

if #arg < 1
  print 'Usage: day12 <input>'
  os.exit 1

input = [parse line for line in io.lines arg[1] when line != '']
part1 = 0
part2 = 0
i = 1

for {pattern, lengths} in *input
  print "[#{i}/#{#input}] Solving #{pattern} with #{#lengths} groups"

  part1 += solve pattern, lengths

  longPattern = "#{pattern}?#{pattern}?#{pattern}?#{pattern}?#{pattern}"
  longLengths = [length for i = 1, 5 for length in *lengths]
  part2 += solve longPattern, longLengths

  i += 1

print "Part 1: #{part1}"
print "Part 2: #{part2}"
