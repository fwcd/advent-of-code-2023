if #arg < 1
  print "Usage: day12 <input>"
  os.exit 1

input = [line for line in io.lines arg[1]]

for line in *input
  print line
