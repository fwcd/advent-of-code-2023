using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

int Mod(int n, int m) => (n % m + m) % m;

List<long> Differences(IEnumerable<long> list) =>
  list
    .Zip(list.Skip(1)).Select(p => p.Item2 - p.Item1)
    .ToList();

bool IsWall(long cell) => cell < 0;

long Get(List<List<long>> maze, Pos pos) =>
  maze[Mod(pos.Y, maze.Count)][Mod(pos.X, maze[0].Count)];

string Pretty(List<List<long>> maze) =>
  string.Join("\n", maze
    .Select(row => string.Join(", ", row
      .Select(c => IsWall(c) ? " " : c.ToString()))));

Pos StartPos(List<List<long>> maze) =>
  maze
    .SelectMany((row, y) => row
      .Select(Pos? (cell, x) => cell == 1 ? new Pos(x, y) : null))
    .First(pos => pos != null)!
    .Value;

HashSet<Pos> Step(List<List<long>> maze, HashSet<Pos> current, int steps) =>
  Enumerable.Range(0, steps)
    .Aggregate(current, (current, i) => current
      .SelectMany(pos => pos.Neighbors)
      .Where(pos => !IsWall(Get(maze, pos)))
      .ToHashSet());

long OccupiedCountNaive(List<List<long>> maze, int steps)
{
  var current = new HashSet<Pos> {StartPos(maze)};
  current = Step(maze, current, steps);
  return current.Count;
}

long OccupiedCountSmart(List<List<long>> maze, int steps)
{
  // Compute the naive occupied count in multiples of the maze side length.
  // We then have a quadratic sequence that we can extrapolate.
  var current = new HashSet<Pos> {StartPos(maze)};
  current = Step(maze, current, maze.Count / 2 - 1);
  var counts = new List<long>();
  for (int i = 0; i < 4; i++)
  {
    counts.Add(current.Count);
    Console.WriteLine($"Got {current.Count}");
    current = Step(maze, current, maze.Count);
  }
  var diffs = Differences(counts);
  var diffs2 = Differences(diffs);
  Console.WriteLine(string.Join(", ", diffs2.Select(n => n.ToString())));
  return -1; // TODO
}

if (args.Length == 0)
{
  Console.WriteLine("Usage: day21 <path to input>");
  return 1;
}

List<List<long>> maze = File.ReadAllText(args[0])
  .Trim()
  .Split('\n')
  .Select(row => row
    .Select(cell => cell == 'S' ? 1L : cell == '#' ? -1L : 0L)
    .ToList())
  .ToList();

long part1 = OccupiedCountNaive(maze, 64);
Console.WriteLine($"Part 1: {part1}");

long part2 = OccupiedCountSmart(maze, 64);
Console.WriteLine($"Part 2: {part2}");

return 0;

public record struct Pos(int X, int Y)
{
  public IEnumerable<Pos> Neighbors
  {
    get
    {
      int x = X;
      int y = Y;
      return Enumerable.Range(-1, 3)
        .SelectMany(d => d == 0 ? new List<Pos>() : new List<Pos> {
          new Pos(x + d, y),
          new Pos(x, y + d),
        });
    }
  }
  
  public static Pos operator+(Pos lhs, Pos rhs) => new Pos(lhs.X + rhs.X, lhs.Y + rhs.Y);

  public static Pos operator-(Pos lhs, Pos rhs) => new Pos(lhs.X - rhs.X, lhs.Y - rhs.Y);
}
