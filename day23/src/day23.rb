#!/usr/bin/env ruby

require 'set'

def get(matrix, pos)
  if pos[0] >= 0 && pos[0] < matrix.size && pos[1] >= 0 && pos[1] < matrix[0].size
    matrix[pos[0]][pos[1]]
  end
end

def neighbors(matrix, pos, slopes: true)
  [-1, 1]
    .flat_map { |d| [[d, 0], [0, d]] }
    .map { |d| [d, pos.zip(d).map { |a| a.inject(:+) }] }
    .filter do |a|
      v = get(matrix, a[1])
      v == '.' || (
        ((!slopes || a[0] == [ 0,  1]) && v == '>') ||
        ((!slopes || a[0] == [ 0, -1]) && v == '<') ||
        ((!slopes || a[0] == [ 1,  0]) && v == 'v') ||
        ((!slopes || a[0] == [-1,  0]) && v == '^')
      )
    end
    .map { |a| a[1] }
end

START = [0, 1]

def compute_graph(matrix, slopes: true)
  visited = Set[]
  adjacent = {}
  origins = {}
  remaining = [[nil, START]]

  while !(pred, start = remaining.shift).nil?
    visited.add(start)
    last = pred
    pos = start
    w = 1
    while (neighs = neighbors(matrix, pos, slopes: slopes).filter { |n| n != last }).size == 1
      # Follow path
      last = pos
      pos = neighs[0]
      visited.add(pos)
      origins[pos] = start
      w += 1
    end
    # If neighs.size > 1 we're at a choice point, otherwise at the end
    pred ||= start
    adjacent[pred] = (adjacent[pred] || {}).merge({pos => w})
    remaining += neighs.filter { |n| !visited.include?(n) }.map { |n| [pos, n] }
  end

  unless slopes
    # Insert missing reverse edges
    adjacent = adjacent
      .flat_map { |pos, es| es.flat_map { |n, w| [[pos, n, w], [n, pos, w]] } }
      .group_by { |e| e[0] }
      .map { |pos, es| [pos, es.map { |e| e[1..] }.to_h] }
      .to_h

    # Remove directed back-edges along the boundary (i.e. those with 3
    # neighbors) via a BFS, these would never lead to a successful path.
    # Credits go to u/maneatingape for discovering this trick (and for making me
    # research self-avoiding rook walks):
    # https://www.reddit.com/r/adventofcode/comments/18oy4pc/comment/kfyvp2g
    remaining = [START]
    visited = Set[]
    while !(pos = remaining.shift).nil?
      visited.add(pos)
      neighs = adjacent[pos].keys.filter { |n| !visited.include?(n) && adjacent[n].size <= 3 }
      neighs.each { |n| adjacent[n].delete(pos) }
      remaining += neighs
    end
  end

  adjacent
end

def dotify_graph(adjacent)
  ids = (adjacent.keys + adjacent.values.map { |es| es.keys }.flatten(1)).to_set.each_with_index.map { |pos, i| [pos, "#{i}"] }.to_h
  [
    'digraph {',
    *ids.map do |pos, i|
      "#{i} [label=\"#{pos}\", pos=\"#{pos[1]},-#{pos[0]}!\"];"
    end,
    *adjacent.flat_map do |pos, es|
      es.map do |n, w|
        "#{ids[pos]} -> #{ids[n]} [label=\"#{w}\"];"
      end
    end,
    '}',
  ].join("\n")
end

def longest_path(
  adjacent,
  pos = START,
  goal = adjacent.flat_map { |_, es| es.keys }.max_by { |p| p.sum },
  visited = Set[],
  depth = 0
)
  if depth < 5
    puts('|' * visited.size)
  end
  es = (adjacent[pos] || {}).filter { |n, _| !visited.include?(n) }
  if es.empty?
    if pos == goal
      0
    else
      -1_000_000_000
    end
  else
    es.map { |n, w| w + longest_path(adjacent, n, goal, visited | Set[pos], depth + 1) }.max || 0
  end
end

opts, vals = ARGV.partition { |a| a.start_with?('--') }

if vals.size == 0 || opts.include?('--help')
  puts [
    "Usage: day23 [--dot] <path to input>",
    "",
    "To visualize the graph, make sure to have GraphViz installed",
    "and pipe the output to a layout engine that supports fixed",
    "positioning, e.g. neato:",
    "",
    "    day23 --dot <path to input> | neato -Tpdf -o <path to output pdf>",
    "",
    "",
  ].join("\n")
  exit 1
end

matrix = File.readlines(vals[0])
  .map { |l| l.strip }
  .filter { |l| !l.empty? }

if opts.include?('--dot')
  puts dotify_graph(compute_graph(matrix, slopes: false))
else
  puts "Part 1: #{longest_path(compute_graph(matrix, slopes: true)) - 1}"
  puts "Part 2: #{longest_path(compute_graph(matrix, slopes: false)) - 1}"
end
