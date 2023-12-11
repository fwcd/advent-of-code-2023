unless ARGV.size >= 1
  puts "Usage: day11 <input>"
  exit 1
end

def expand_vertically(lines)
  lines.flat_map do |line|
    if line.includes?('#')
      [line]
    else
      [line, line]
    end
  end
end

def transpose(lines)
  (0...lines[0].size).map do |x|
    lines.map do |line|
      line[x]
    end.join
  end
end

def expand_2d(lines)
  lines = expand_vertically(lines)
  lines = transpose(lines)
  lines = expand_vertically(lines)
  transpose(lines)
end

def find_galaxies(lines)
  lines.map_with_index do |line, y|
    line.chars.map_with_index do |c, x|
      if c == '#'
        [x, y]
      end
    end.compact
  end.flat_map &.itself
end

def manhattan(pos1, pos2)
  pos1.zip(pos2).map { |ps| (ps[1] - ps[0]).abs }.sum
end

def solve(positions)
  positions.map_with_index do |pos1, i|
    positions[(i + 1)..].map_with_index do |pos2, j|
      manhattan(pos1, pos2)
    end.sum
  end.sum
end

path = ARGV[0]
lines = File.read_lines(path)

lines = expand_2d(lines)
positions = find_galaxies(lines)
part1 = solve(positions)

puts "Part 1: #{part1}"
