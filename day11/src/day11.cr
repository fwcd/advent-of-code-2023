unless ARGV.size >= 1
  puts "Usage: day11 <input>"
  exit 1
end

alias Position = Array(Int32)

def expand_1d(positions : Array(Position), axis : Int32) : Array(Position)
  sorted = positions.sort_by { |pos| pos[axis] }
  sorted.reduce({0, [] of Position}) do |acc, pos|
    offset, acc_positions = acc
    unless acc_positions.empty?
      delta = pos[axis] + offset - acc_positions.last[axis]
      if delta > 1
        offset += delta - 1
      end
    end
    if offset > 0
      pos = [*pos]
      pos[axis] += offset
    end
    {offset, [*acc_positions, pos]}
  end[1]
end

def expand_2d(positions : Array(Position)) : Array(Position)
  expand_1d(expand_1d(positions, 1), 0)
end

def find_galaxies(lines : Array(String)) : Array(Position)
  lines.map_with_index do |line, y|
    line.chars.map_with_index do |c, x|
      if c == '#'
        [x, y]
      end
    end.compact
  end.flat_map &.itself
end

def manhattan(pos1 : Position, pos2 : Position) : Int32
  pos1.zip(pos2).map { |ps| (ps[1] - ps[0]).abs }.sum
end

def solve(positions : Array(Position)) : Int32
  positions.map_with_index do |pos1, i|
    positions[(i + 1)..].map_with_index do |pos2, j|
      manhattan(pos1, pos2)
    end.sum
  end.sum
end

path = ARGV[0]
lines = File.read_lines(path)

positions = find_galaxies(lines)
positions = expand_2d(positions)
part1 = solve(positions)

puts "Part 1: #{part1}"
