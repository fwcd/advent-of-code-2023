unless ARGV.size >= 1
  puts "Usage: day11 <input>"
  exit 1
end

path = ARGV[0]
input = File.read_lines(path)

map = input.flat_map do |line|
  if line.includes?('#')
    [line]
  else
    [line, line]
  end
end

positions = map.map_with_index do |line, y|
  line.chars.map_with_index do |c, x|
    if c == '#'
      [x, y]
    end
  end.compact
end.flat_map &.itself

puts positions
