unless ARGV.size >= 1
  puts "Usage: day11 <input>"
  exit 1
end

path = ARGV[0]
input = File.read_lines(path)
puts input
