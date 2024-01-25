#!/usr/bin/env ruby

if ARGV.size == 0
  puts "Usage: day23 <path to input>"
  exit 1
end

matrix = File.readlines(ARGV[0])
puts matrix
