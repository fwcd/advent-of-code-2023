using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

int Mod(int n, int m) => (n % m + m) % m;

bool IsWall(long cell) => cell < 0;

long Get(List<List<long>> maze, Pos pos) =>
  maze[Mod(pos.Y, maze.Count)][Mod(pos.X, maze[0].Count)];

string Pretty(List<List<long>> maze) =>
  string.Join("\n", maze
    .Select(row => string.Join(", ", row
      .Select(c => IsWall(c) ? " " : c.ToString()))));

long OccupiedCount(List<List<long>> maze, int steps)
{
  Pos start = maze
    .SelectMany((row, y) => row
      .Select(Pos? (cell, x) => cell == 1 ? new Pos(x, y) : null))
    .First(pos => pos != null)!
    .Value;
  var current = new HashSet<Pos>();
  current.Add(start);
  for (int i = 0; i < steps; i++)
  {
    current = current
      .SelectMany(pos => pos.Neighbors)
      .Where(pos => !IsWall(Get(maze, pos)))
      .ToHashSet();
  }
  return current.Count;
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

long part1 = OccupiedCount(maze, 64);

Console.WriteLine($"Part 1: {part1}");

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
