#!/usr/bin/env ruby

require 'set'

def get(matrix, pos)
  if pos[0] >= 0 && pos[0] < matrix.size && pos[1] >= 0 && pos[1] < matrix[0].size
    matrix[pos[0]][pos[1]]
  end
end

def neighbors(matrix, pos)
  [-1, 1]
    .flat_map { |d| [[d, 0], [0, d]] }
    .map { |d| [d, pos.zip(d).map { |a| a.inject(:+) }] }
    .filter do |a|
      v = get(matrix, a[1])
      v == '.' ||
      (a[0] == [ 0,  1] && v == '>') ||
      (a[0] == [ 0, -1] && v == '<') ||
      (a[0] == [ 1,  0] && v == 'v') ||
      (a[0] == [-1,  0] && v == '^')
    end
    .map { |a| a[1] }
end

def longest_path(matrix, visited = Set[], pos = [0, 1])
  visited.add(pos)

  # Follow path until we reach a choice point or the end
  while (n = neighbors(matrix, pos).filter { |p| !visited.include?(p) }).size == 1
    pos = n[0]
    visited.add(pos)
  end

  if n.size == 0
    visited.size - 1
  else
    n.map { |p| longest_path(matrix, visited.clone, p) }.max
  end
end

if ARGV.size == 0
  puts "Usage: day23 <path to input>"
  exit 1
end

matrix = File.readlines(ARGV[0])
puts "Part 1: #{longest_path(matrix)}"
